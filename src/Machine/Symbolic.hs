module Machine.Symbolic where

import Control.Selective
import Control.Monad.State
import qualified Data.Map.Strict as Map

import Machine.Types
import Machine.Encode
import Machine.Decode
import Machine.Semantics
import qualified Data.Tree as Tree

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

-- | The semantics for JumpZero for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpZeroSym :: SImm8 -> SymEngine ()
jumpZeroSym simm =
    whenSym (readKey (F Zero))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

symStep :: SymState -> [SymState]
symStep state =
    let [(instrCode, fetched)] = (flip runSymEngine) state $ do
                                    fetchInstruction
                                    incrementInstructionCounter
                                    readInstructionRegister
        i = decode instrCode
    in -- (snd . ((flip runSymEngine) fetched)) <$>
       case i of
          (Instruction (JumpZero offset)) -> map snd $ runSymEngine (jumpZeroSym offset) fetched
          _ -> map snd $
            runSymEngine (instructionSemantics i readKey writeKey) fetched

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