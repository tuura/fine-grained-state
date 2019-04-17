{-# LANGUAGE LambdaCase, MultiWayIf, GADTs, TypeFamilies
            , RankNTypes, MultiParamTypeClasses, FlexibleInstances
            , DeriveFunctor, StandaloneDeriving, ScopedTypeVariables
            , ConstraintKinds #-}

module Machine where

import Data.Bits
import Prelude hiding (Read, Monad)
import qualified Prelude (Read, Monad)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State
import Control.Selective
import Data.Functor (void)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16, Word64)
import Data.Int (Int8, Int16, Int64)
import Data.Functor.Const
import Data.Either (partitionEithers)
import qualified Data.Tree as Tree

--------------------------------------------------------------------------------
---------------- Types ---------------------------------------------------------
--------------------------------------------------------------------------------

data Register = R0 | R1 | R2 | R3
    deriving (Show, Prelude.Read, Eq, Ord, Enum)

type MemoryAddress = Word16

type SImm8 = Int8

-- | Boolean 'Flag's indicate the current status of Iam.
data Flag = Zero
          | Overflow
          | Halted
          deriving (Show, Prelude.Read, Eq, Ord, Enum)

type Value = Int16

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Iam.Semantics.wait' instruction.
type Clock = Value

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, InstructionCode)]

--------------------------------------------------------------------------------
---------------- Instructions --------------------------------------------------
--------------------------------------------------------------------------------

class Unconstrained (a :: * -> *)
instance Unconstrained a

data InstructionImpl c where
  Halt   :: InstructionImpl Applicative
  Load   :: Register -> MemoryAddress -> InstructionImpl Functor
  Set    :: Register -> SImm8 -> InstructionImpl Functor
  Store  :: Register -> MemoryAddress -> InstructionImpl Functor
  Add    :: Register -> MemoryAddress -> InstructionImpl Applicative
  -- AddM   :: Register -> MemoryAddress -> InstructionImpl Monad
  Sub    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Mul    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Div    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Mod    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Abs    :: Register -> InstructionImpl Applicative
  Jump :: SImm8 -> InstructionImpl Applicative
  JumpZero :: SImm8 -> InstructionImpl Selective
  LoadMI :: Register -> MemoryAddress -> InstructionImpl Monad

deriving instance Eq  (InstructionImpl c)
deriving instance Ord (InstructionImpl c)

instance Show (InstructionImpl c) where
  show = \case
    Halt            -> "Halt"
    Load  reg addr  -> "Load " ++ show reg ++ " " ++ show addr
    Set   reg value -> "Set " ++ show reg ++ " " ++ show value
    Store reg addr  -> "Store " ++ show reg ++ " " ++ show addr
    Add   reg addr  -> "Add " ++ show reg ++ " " ++ show addr
    Sub   reg addr  -> "Sub " ++ show reg ++ " " ++ show addr
    Mul   reg addr  -> "Mul " ++ show reg ++ " " ++ show addr
    Div   reg addr  -> "Div " ++ show reg ++ " " ++ show addr
    Mod   reg addr  -> "Mod " ++ show reg ++ " " ++ show addr
    Abs   reg       -> "Abs " ++ show reg
    Jump  offset    -> "Jump " ++ show offset
    JumpZero offset    -> "Jump " ++ show offset
    LoadMI reg addr -> "LoadMI " ++ show reg ++ " " ++ show addr

data Instruction = forall c. Instruction (InstructionImpl c)

instance Show Instruction where
  show (Instruction i) = show i

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | Binary representation of an instruction
type InstructionCode = Word16

decode :: InstructionCode -> Instruction
decode code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [f, f, f, f, f, f] -> Instruction Halt
          | opcode == [f, f, f, f, f, t]  -> Instruction $
                Load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, f]   -> Instruction $
                LoadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, t]  -> Instruction $
                Set (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractSImm8 expandedCode)
          | opcode == [f, f, f, t, f, f]   -> Instruction $
                Store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, f, t]   -> Instruction $
                Add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, t, f]   -> Instruction $
                Jump (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, f, f, t, t, t]    -> Instruction $
                JumpZero (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, f, t, f, f, f]   -> Instruction $
                Sub (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, f, t]   -> Instruction $
                Mul (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, f]   -> Instruction $
                Div (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, t]   -> Instruction $
                Mod (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, t, f, f]   -> Instruction $
                Abs (decodeRegister . extractRegister $ expandedCode)
      where f = False
            t = True

fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []     = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

decodeRegister :: [Bool] -> Register
decodeRegister = \case
      [False, False] -> R0
      [False, True]  -> R1
      [True, False]  -> R2
      [True, True]   -> R3
      _              -> error $ "Machine.Instruction.Decode.decodeRegister:"
                             <> "register must be encoded as a two-bit word"

decodeOpcode :: [Bool] -> [Bool]
decodeOpcode = take 6

extractRegister :: [Bool] -> [Bool]
extractRegister = take 2 . drop 6

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = (++ pad 56) . take 8 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = (++ pad 56) . take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = (++ pad 56) . take 8 . drop 6

pad :: Int -> [Bool]
pad k = replicate k False


encode :: Instruction -> InstructionCode
encode = \case
    Instruction Halt -> 0
    Instruction (Load     r addr) ->
        fromBitsLE $ [f, f, f, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (LoadMI   r addr) ->
        fromBitsLE $ [f, f, f, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Set      r byte) ->
        fromBitsLE $ [f, f, f, f, t, t] ++ encodeRegister r
                                        ++ encodeByte byte
                                        ++ pad 48
    Instruction (Store    r addr) ->
        fromBitsLE $ [f, f, f, t, f, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Add      r addr) ->
        fromBitsLE $ [f, f, f, t, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Jump     byte)   ->
        fromBitsLE $ [f, f, f, t, t, f] ++ encodeByte byte
                                        ++ pad 50
    Instruction (JumpZero byte)   ->
        fromBitsLE $ [f, f, f, t, t, t] ++ encodeByte byte
                                        ++ pad 50
    Instruction (Sub      r addr) ->
        fromBitsLE $ [f, f, t, f, f, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Mul      r addr) ->
        fromBitsLE $ [f, f, t, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Div      r addr) ->
        fromBitsLE $ [f, f, t, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Mod      r addr) ->
        fromBitsLE $ [f, f, t, f, t, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Abs      r)      ->
        fromBitsLE $ [f, f, t, t, f, f] ++ encodeRegister r
                                        ++ pad 56
    where f = False
          t = True
          pad k = replicate k f

-- | 'Register' is encoded as a 2-bit word
encodeRegister :: Register -> [Bool]
encodeRegister = \case
    R0 -> [False, False]
    R1 -> [False, True]
    R2 -> [True, False]
    R3 -> [True, True]

-- | 'MemoryAddress' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeMemoryAddress :: MemoryAddress -> [Bool]
encodeMemoryAddress = take 8 . blastLE

-- | 'Byte' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeByte :: SImm8 -> [Bool]
encodeByte = take 8 . blastLE
--------------------------------------------------------------------------------
---------------- Symbolic expressions and state --------------------------------
--------------------------------------------------------------------------------

-- | Symbolic expressions
data Sym a where
    SConst :: a -> Sym a
    SAny   :: Int -> Sym Value
    SAdd   :: Sym Value -> Sym Value -> Sym Value
    SSub   :: Sym Value -> Sym Value -> Sym Value
    SMul   :: Sym Value -> Sym Value -> Sym Value
    SDiv   :: Sym Value -> Sym Value -> Sym Value
    SMod   :: Sym Value -> Sym Value -> Sym Value
    SAbs   :: Sym Value -> Sym Value
    SEq    :: Sym Value -> Sym Value -> Sym Bool
    SGt    :: Sym Value -> Sym Value -> Sym Bool
    SLt    :: Sym Value -> Sym Value -> Sym Bool
    SAnd   :: Sym Bool -> Sym Bool -> Sym Bool
    SOr    :: Sym Bool -> Sym Bool -> Sym Bool
    SNot   :: Sym Bool -> Sym Bool

instance Show a => Show (Sym a) where
    show (SAdd x y)   = "(" <> show x <> " + " <> show y <> ")"
    show (SSub x y)   = "(" <> show x <> " - " <> show y <> ")"
    show (SMul x y)   = "(" <> show x <> " * " <> show y <> ")"
    show (SDiv x y)   = "(" <> show x <> " / " <> show y <> ")"
    show (SMod x y)   = "(" <> show x <> " % " <> show y <> ")"
    show (SAbs x  )   = "|" <> show x <> "|"
    show (SConst x)   = show x
    show (SAnd x y)   = "(" <> show x <> " & " <> show y <> ")"
    show (SOr  x y)   = "(" <> show x <> " | " <> show y <> ")"
    show (SAny n  )   = "val_" <> show n
    show (SEq  x y)   = "(" <> show x <> " == " <> show y <> ")"
    show (SGt  x y)   = "(" <> show x <> " > " <> show y <> ")"
    show (SLt  x y)   = "(" <> show x <> " < " <> show y <> ")"
    show (SNot b )    = "¬" <> show b

instance Eq a => Eq (Sym a) where
    (SConst x) == (SConst y) = x == y
    _ == _ = error "Sym.Eq: can't compare non-constant symbolic values"
--------------------------------------------------------------------------------
---------------- Instruction Semantics -----------------------------------------
--------------------------------------------------------------------------------

data Key a where
    Reg  :: Register -> Key (Value)
    -- ^ register
    Addr :: MemoryAddress -> Key (Value)
    -- ^ memory address
    F   :: Flag -> Key (Bool)
    -- -- ^ flag
    IC   :: Key (Value)
    -- -- ^ instruction counter
    IR   :: Key (InstructionCode)
    -- ^ instruction register
    Prog :: InstructionAddress -> Key (InstructionCode)
    -- ^ program memory address

instance Show (Key a) where
    show = \case
        Reg  reg   -> show reg
        Addr addr  -> show addr
        F    flag  -> show flag
        IC         -> "IC"
        IR         -> "IR"
        Prog addr  -> show addr

type Read f = forall a. Key a -> f (Sym a)

type Write f = forall a. Key a -> f (Sym a) -> f (Sym a)

-- type Read f = forall a. Key a -> f (a)

-- type Write f = forall a. Key a -> f (a) -> f (a)

type FS c a = forall f. c f => Read f ->
                               Write f ->
                               f a

-- type FS c a = forall f. c f => (forall v. Key v -> f (Sym v)) ->
--                                (forall v. Key v -> f (Sym v)  -> f (Sym v)) ->
--                                f a

getEffects :: FS Selective a -> ([String], [String])
getEffects task =
    partitionEithers . getConst $
    task trackingRead trackingWrite
    where
        trackingRead k     = Const [Left (show k)]
        trackingWrite k fv = fv *> Const [Right (show k)]

-- t :: FS Applicative ()
-- t = \r w -> add R0 0 r w *> add R1 1 r w

-- | Halt the execution.
--   Applicative.
halt :: FS Applicative ()
halt read write = void $
    write (F Halted) (pure (SConst True))

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress
    -> FS Applicative ()
add reg addr = \read write -> void $
    let result = SAdd <$> read (Reg reg) <*> read (Addr addr)
    in write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> FS Functor ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative ()
sub reg addr = \read write -> void $
    let result = (SSub) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> FS Applicative ()
mul reg addr = \read write -> void $
    let result = (SMul) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> FS Applicative ()
div reg addr = \read write -> void $
    let result = SDiv <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

mod :: Register -> MemoryAddress -> FS Applicative ()
mod reg addr = \read write -> void $
    let result = SMod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

abs :: Register -> FS Functor ()
abs reg = \read write -> void $
    let result = SAbs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor ()
jump simm read write = void $
    write IC (fmap ((+) . fromIntegral $ simm) (read IC))

-- | The semantics for JumpZero for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSYm (instead of desired Selective whenS).
jumpZero :: SImm8 -> SymEngine ()
jumpZero simm =
    whenSym (readKey (F Zero))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

-- | Retrieve all leaf-nodes of the symbolic expression
unsafeLeafs :: Sym a -> [Sym a]
unsafeLeafs = go []
    where go :: [Sym a] -> Sym a -> [Sym a]
          go xs v = case v of
             (SConst x) -> v:xs
             (SAdd p q) -> go (go xs p) q

-- | Assume that a symbolic expression only contains constant leafs and fold it into a single
--   constant leaf
unsafeFoldSConst :: Sym Value -> Sym Value
unsafeFoldSConst = SConst . foldr (\(SConst x) acc -> x + acc) 0 . unsafeLeafs

instructionSemantics :: Instruction -> FS Selective ()
instructionSemantics (Instruction i) r w = case i of
    Halt -> halt r w
    Load reg addr -> load reg addr r w
    -- -- LoadMI reg addr -> loadMI reg addr r w
    -- Set reg simm8  -> setF reg simm8 r w
    -- Store reg addr -> store reg addr r w
    Add reg addr   -> add reg addr r w
    -- Sub reg addr   -> sub reg addr r w
    -- Mul reg addr   -> mul reg addr r w
    -- Div reg addr   -> div reg addr r w
    -- Mod reg addr   -> mod reg addr r w
    -- Abs reg        -> abs reg r w
    -- Jump simm8     -> jump simm8 r w
    JumpZero simm8 -> undefined -- jumpZero simm8 r w

--------------------------------------------------------------------------------
---------------- Simulator -----------------------------------------------------
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
---------------- Symbolic Engine -----------------------------------------------
--------------------------------------------------------------------------------

-- | The state of symbolic computation
data SymState = SymState { registers         :: Map.Map Register (Sym Value)
                         , instructionCounter :: InstructionAddress -- Sym
                         , instructionRegister :: InstructionCode
                         , flags :: Map.Map Flag (Sym Bool)
                         , memory :: Map.Map MemoryAddress (Sym Value)
                         , program :: Program
                         , clock :: Clock
                         , pathConstraintList :: [Sym Bool]
                         }

renderSymState :: SymState -> String
renderSymState state =
  "IC: " <> show (instructionCounter state) <> "\n" <>
  "IR: " <> show (decode $ instructionRegister state) <> "\n" <>
  "Registers: " <> show (registers state) <> "\n" <>
  "Flags: " <> show (Map.toList $ flags state) <> "\n" <>
  "Path Constraints: \n" <> renderPathConstraints (pathConstraintList state) <> "\n"
  where
    renderPathConstraints :: Show a => [Sym a] -> String
    renderPathConstraints xs = foldr (\x acc -> " && " <> show x <> "\n" <> acc) "" xs

emptyRegisters :: Map.Map Register (Sym Value)
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] (map SConst [0, 0..])

emptyFlags :: Map.Map Flag (Sym Bool)
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map SConst $ repeat False)

initialiseMemory :: [(MemoryAddress, Sym Value)] -> Map.Map MemoryAddress (Sym Value)
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
    in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

boot :: Program -> Map.Map MemoryAddress (Sym Value) -> SymState
boot prog mem = SymState { registers = emptyRegisters
                         , instructionCounter = 0
                         , instructionRegister = encode . Instruction $ Jump 0
                         , program = prog
                         , flags = emptyFlags
                         , memory = mem
                         , clock = 0

                         , pathConstraintList = []
                         }


-- | The symbolic execution trace
type Trace = Tree.Tree SymState

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data SymEngine a = SymEngine
    { runSymEngine :: SymState -> [(a, SymState)] }
    deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative SymEngine where
    pure  = return
    (<*>) = ap

-- | Conditionally perform an effect.
whenSym :: SymEngine (Sym Bool) -> SymEngine () -> SymEngine ()
whenSym cond comp = -- select (bool (Right ()) (Left ()) <$> x) (const <$> y)
    SymEngine $ \s -> do
        a@(evalCond, s') <- runSymEngine cond s
        b@(compRes, s'') <- runSymEngine comp s'
        (f a (snd b))
        -- pure ((), appendConstraints [evalCond] s')
        -- pure ((), appendConstraints [(SNot evalCond)] s'')
    -- concat [f a (snd b) | a <- runSymEngine cond s, b <- runSymEngine comp (snd a)]
    where
        f :: (Sym Bool, SymState) -> SymState -> [((), SymState)]
        f (b, sNoExec) sOnExec =
            [ ((), appendConstraints [b] sOnExec)
            , ((), appendConstraints [(SNot b)] sNoExec)]

instance Selective SymEngine where
    select = selectM

instance Prelude.Monad SymEngine where
    return a       = SymEngine $ \s -> [(a, s)]
    SymEngine r >>= f = SymEngine $ \s ->
        let outcomes = r s
        in concat $ map (\(result, state) -> runSymEngine (f result) state) outcomes

instance (MonadState SymState) SymEngine where
    get   = SymEngine $ \s -> [(s, s)]
    put s = SymEngine $ \_ -> [((), s)]

appendConstraints :: [Sym Bool] -> SymState -> SymState
appendConstraints cs s =
    let cs' = cs ++ pathConstraintList s
    in s { pathConstraintList = cs' }

symStep :: SymState -> [SymState]
symStep state =
    let [(instrCode, fetched)] = (flip runSymEngine) state $ do
                                    fetchInstruction
                                    incrementInstructionCounter
                                    readInstructionRegister
        i = decode instrCode
    in -- (snd . ((flip runSymEngine) fetched)) <$>
       case i of
          (Instruction (JumpZero offset)) -> map snd $ runSymEngine (jumpZero offset) fetched
          _ -> map snd $
            runSymEngine (instructionSemantics i readKey writeKey) fetched

runModel :: Int -> SymState -> Trace
runModel steps state
    | steps <= 0 = Tree.Node state []
    | otherwise  = if halted then Tree.Node state [] else Tree.Node state children
  where
    halted    = (Map.!) (flags state) Halted == (SConst True)
    newStates = symStep state
    children  = runModel (steps - 1) <$> newStates

-- | Instance of the Machine.Metalanguage read command for symbolic execution
readKey :: Key a -> SymEngine (Sym a)
readKey = \case
    Reg  src  -> (Map.!) <$> (registers <$> get) <*> pure src
    Addr src  -> (Map.!) <$> (memory    <$> get) <*> pure src
    F    flag -> (Map.!) <$> (flags     <$> get) <*> pure flag
    IC        -> SConst . instructionCounter  <$> get
    IR        -> SConst . instructionRegister <$> get
    Prog addr -> SConst <$> snd <$> ((!!) <$> (program <$> get) <*> pure (fromIntegral addr))


-- | Instance of the Machine.Metalanguage write command for symbolic execution
writeKey :: Key a
         -> SymEngine (Sym a)
         -> SymEngine (Sym a)
writeKey k v = case k of
    Reg  target -> v >>= (\x -> writeRegister target x *> pure x)
    Addr target -> v >>= (\x -> writeMemory   target x *> pure x)
    F    flag   -> v >>= (\x -> writeFlag flag x *> pure x)
    IC          -> v >>= \x -> case (unsafeFoldSConst x) of
        (SConst val) -> (modify $ \s -> s {instructionCounter = val}) *> pure (SConst val)
        _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IC is not supported"
    IR        -> v >>= \case
        (SConst val) -> writeInstructionRegister val *> pure (SConst val)
        _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IR is not supported"
    Prog _    -> error "Machine.Semantics.Symbolic: Can't write Program"

-- | Increment the instruction counter.
incrementInstructionCounter :: SymEngine ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = (+) 1 (instructionCounter currentState)}

fetchInstruction :: SymEngine ()
fetchInstruction =
    snd <$> ((!!) <$> (program <$> get) <*> (fromIntegral . instructionCounter <$> get)) >>=
    writeInstructionRegister

readInstructionRegister :: SymEngine InstructionCode
readInstructionRegister = instructionRegister <$> get

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Sym Value -> SymEngine ()
writeRegister target value =
    modify $ \s ->
        s {registers = Map.adjust (const value) target (registers s)}

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> Sym Value -> SymEngine ()
writeMemory target value =
    modify $ \s ->
        s {memory = Map.adjust (const value) (fromIntegral target) (memory s)}

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Sym Bool -> SymEngine ()
writeFlag flag value = do
    modify $ \s ->
        s { flags = Map.adjust (const value) flag (flags s) }

writeInstructionRegister :: InstructionCode -> SymEngine ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}

----------------------------------------------------------------------------------------------------
------------------- Examples -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

-- | Quick-and-dirty program parser.
--   Comments start with the '#' character.
--   Blank lines are ignored.
parseProgram :: String -> Program
parseProgram src =
    let instructions = map read . removeBlankLines . removeComments . lines $ src
    in addInstructionAddresses instructions
    where removeComments = map (takeWhile (/= '#'))
          removeBlankLines = filter (not . null)
          addInstructionAddresses = zip [0..]

showProgram :: Program -> String
showProgram prog = let is = map snd $ prog
                   in unlines . map show $ is

addExample :: IO ()
addExample = do
    let prog = zip [0..] $ map encode
          [ Instruction (Load R0 0)
          , Instruction (Add R0 1)
          , Instruction (Halt)
          , Instruction (Halt)
          ]
        steps = 15
        -- x = SConst 2
        -- y = SConst 3
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        trace = runModel steps initialState
    print prog
    putStrLn $ Tree.drawTree $ fmap renderSymState $ trace

-- gcdExample :: IO ()
-- gcdExample = do
--     let prog = zip [1..] $ map encode
--           [ Instruction (Set R0 0)
--           , Instruction (Store R0 255)
--           , Instruction (Load R0 1)
--           , Instruction (Sub R0 255)
--           , Instruction (JumpZero 6)
--           , Instruction (Load R0 0)
--           , Instruction (Mod R0 1)
--           , Instruction (Load R1 1)
--           , Instruction (Store R0 1)
--           , Instruction (Store R1 0)
--           , Instruction (Jump (-8))
--           , Instruction Halt ]
--         steps = 15
--         x = SConst 2
--         y = SConst 3
--         -- x = SAny 0
--         -- y = SAny 1
--         mem = initialiseMemory [(0, x), (1, y)]
--         initialState = boot prog mem
--         trace = runModel steps initialState
--     print prog
--     putStrLn $ Tree.drawTree $ fmap renderSymState $ trace