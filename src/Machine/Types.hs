{-# LANGUAGE ExistentialQuantification #-}
module Machine.Types where

import           Control.Selective
import           Data.Int          (Int16, Int64, Int8)
import           Data.Kind         (Constraint)
import           Data.Word         (Word16, Word64, Word8)
import           Prelude           hiding (Monad, Read, abs, div, mod)
import qualified Prelude           (Monad, Read, abs, div, mod)

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad (m :: * -> *) = (Selective m, Prelude.Monad m)

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
          | Condition
          deriving (Show, Prelude.Read, Eq, Ord, Enum)

type Value = Int64

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

data InstructionImpl (c :: (* -> *) -> Constraint) where
  Halt     :: InstructionImpl Applicative
  Load     :: Register -> MemoryAddress -> InstructionImpl Functor
  Set      :: Register -> SImm8         -> InstructionImpl Functor
  Store    :: Register -> MemoryAddress -> InstructionImpl Functor
  Add      :: Register -> MemoryAddress -> InstructionImpl Applicative
  Sub      :: Register -> MemoryAddress -> InstructionImpl Applicative
  Mul      :: Register -> MemoryAddress -> InstructionImpl Applicative
  Div      :: Register -> MemoryAddress -> InstructionImpl Applicative
  Mod      :: Register -> MemoryAddress -> InstructionImpl Applicative
  Abs      :: Register                  -> InstructionImpl Applicative
  Jump     :: SImm8                     -> InstructionImpl Applicative
  JumpZero :: SImm8                     -> InstructionImpl Selective
  LoadMI   :: Register -> MemoryAddress -> InstructionImpl Prelude.Monad

  CmpEq    :: Register  -> MemoryAddress -> InstructionImpl Selective
  CmpGt    :: Register  -> MemoryAddress -> InstructionImpl Selective
  CmpLt    :: Register  -> MemoryAddress -> InstructionImpl Selective

  JumpCt   :: SImm8                      -> InstructionImpl Selective
  JumpCf   :: SImm8                      -> InstructionImpl Selective

deriving instance Eq  (InstructionImpl c)
deriving instance Ord (InstructionImpl c)

instance Show (InstructionImpl c) where
  show = \case
    Halt               -> "Halt"
    Load     reg addr  -> "Load "     ++ show reg ++ " " ++ show addr
    Set      reg value -> "Set "      ++ show reg ++ " " ++ show value
    Store    reg addr  -> "Store "    ++ show reg ++ " " ++ show addr
    Add      reg addr  -> "Add "      ++ show reg ++ " " ++ show addr
    Sub      reg addr  -> "Sub "      ++ show reg ++ " " ++ show addr
    Mul      reg addr  -> "Mul "      ++ show reg ++ " " ++ show addr
    Div      reg addr  -> "Div "      ++ show reg ++ " " ++ show addr
    Mod      reg addr  -> "Mod "      ++ show reg ++ " " ++ show addr
    Abs      reg       -> "Abs "      ++ show reg
    Jump     offset    -> "Jump "     ++ show offset
    JumpZero offset    -> "JumpZero " ++ show offset
    JumpCt   offset    -> "JumpCt "   ++ show offset
    JumpCf   offset    -> "JumpCf "   ++ show offset
    LoadMI   reg addr  -> "LoadMI "   ++ show reg ++ " " ++ show addr

    CmpEq    reg addr  -> "CmpEq " ++ show reg ++ " " ++ show addr
    CmpGt    reg addr  -> "CmpGt " ++ show reg ++ " " ++ show addr
    CmpLt    reg addr  -> "CmpLt " ++ show reg ++ " " ++ show addr

data Instruction = forall c. Instruction (InstructionImpl c)

instance Show Instruction where
  show (Instruction i) = show i

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | Binary representation of an instruction
type InstructionCode = Word16

--------------------------------------------------------------------------------
---------------- Symbolic expressions and state --------------------------------
--------------------------------------------------------------------------------

-- | Symbolic expressions
data Sym a where
    SConst :: a -> Sym a
    SAny   :: String -> Sym Value
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

-- deriving instance Show a => Show (Sym a)

instance Show a => Show (Sym a) where
    show (SAdd x y) = "(" <> show x <> " + " <> show y <> ")"
    show (SSub x y) = "(" <> show x <> " - " <> show y <> ")"
    show (SMul x y) = "(" <> show x <> " * " <> show y <> ")"
    show (SDiv x y) = "(" <> show x <> " / " <> show y <> ")"
    show (SMod x y) = "(" <> show x <> " % " <> show y <> ")"
    show (SAbs x  ) = "|" <> show x <> "|"
    show (SConst x) = show x
    show (SAnd x y) = "(" <> show x <> " & " <> show y <> ")"
    show (SOr  x y) = "(" <> show x <> " | " <> show y <> ")"
    show (SAny n  ) = n
    show (SEq  x y) = "(" <> show x <> " == " <> show y <> ")"
    show (SGt  x y) = "(" <> show x <> " > " <> show y <> ")"
    show (SLt  x y) = "(" <> show x <> " < " <> show y <> ")"
    show (SNot b )  = "¬" <> show b

instance Eq a => Eq (Sym a) where
    (SConst x) == (SConst y) = x == y
    (SAny   x) == (SAny y)   = x == y
    _ == _ = error "Sym.Eq: can't compare non-trivial symbolic expressions symbolic values"

instance Ord (Sym Value) where
    (SAny l) <= (SAny r) = l <= r
    _ <= _               = error "Sym.Ord: Sym is only ordered by ids of symbolic variables."

instance Bounded a => Bounded (Sym a) where
  minBound = SConst minBound
  maxBound = SConst maxBound

nonZero :: Sym Value -> Bool
nonZero = \case
  (SConst 0) -> True
  _          -> False

-- | Unsafely extract a value of a symbolic expression if it is, in fact,
--   a constant; throw an error if it is not.
unliteral :: Sym a -> a
unliteral = \case
    (SConst x) -> x
    _          -> error "unliteral: can only extract a value from SConst"


-- | Try to perform constant folding and get the resulting value. Return 'Nothing' on
--   encounter of a symbolic variable.
getValue :: Sym a -> Maybe a
getValue = \case
    (SAny   _) -> Nothing
    (SConst x) -> Just x
    (SAdd p q) -> (+)           <$> getValue p <*> getValue q
    (SSub p q) -> (-)           <$> getValue p <*> getValue q
    (SMul p q) -> (*)           <$> getValue p <*> getValue q
    (SDiv p q) -> (Prelude.div) <$> getValue p <*> getValue q
    (SMod p q) -> (Prelude.mod) <$> getValue p <*> getValue q
    (SAbs x  ) -> Prelude.abs   <$> getValue x
    (SAnd p q) -> (&&)          <$> getValue p <*> getValue q
    (SOr  p q) -> (||)          <$> getValue p <*> getValue q
    (SNot x  ) -> not           <$> getValue x
    (SEq  p q) -> (==)          <$> getValue p <*> getValue q
    (SGt  p q) -> (>)           <$> getValue p <*> getValue q
    (SLt  p q) -> (<)           <$> getValue p <*> getValue q

-- | Constant-fold the expression if it only contains 'SConst' leafs; return the
--   unchanged expression otherwise.
tryFoldConstant :: Sym a -> Sym a
tryFoldConstant x =
  let maybeVal = getValue x
  in case maybeVal of
          Just val -> SConst val
          Nothing  -> x

tryReduce :: Sym a -> Sym a
tryReduce = \case
    SNot x -> SNot (tryReduce x)

    -- 0 + y = y
    (SAdd (SConst 0) y) -> tryReduce y
    -- x + 0 = x
    (SAdd x (SConst 0)) -> tryReduce x
    (SAdd x y) -> tryReduce x `SAdd` tryReduce y
    -- x - 0 = x
    (SSub x (SConst 0)) -> tryReduce x
    (SSub x y) -> tryReduce x `SSub` tryReduce y
    -- T && y = y
    (SAnd (SConst True) y) -> tryReduce y
    -- x && T = x
    (SAnd x (SConst True)) -> tryReduce x
    (SAnd x y            ) -> tryReduce x `SAnd` tryReduce y
    -- F || y = y
    (SOr (SConst False) y) -> tryReduce y
    -- x || F = x
    (SOr x (SConst False)) -> tryReduce x
    (SOr x y) -> tryReduce x `SOr` tryReduce y

    (SEq (SConst 0) (SConst 0)) -> SConst True
    (SEq x y) -> tryReduce x `SEq` tryReduce y
    (SGt (SConst 0) (SConst 0)) -> SConst False
    (SGt x y) -> tryReduce x `SGt` tryReduce y
    (SLt (SConst 0) (SConst 0)) -> SConst False
    (SLt x y) -> tryReduce x `SLt` tryReduce y
    s -> s

simplify :: Int -> Sym a -> Sym a
simplify steps | steps <= 0  = id
               | otherwise   = last . take steps
                             . iterate (tryFoldConstant . tryReduce)
