module Machine.Types where

import Data.Kind (Constraint)
import Prelude hiding (Read, Monad, div, mod, abs)
import qualified Prelude (Read, Monad, div, mod, abs)
import Data.Word (Word8, Word16, Word64)
import Data.Int (Int8, Int16, Int64)
import Control.Selective

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

data InstructionImpl (c :: (* -> *) -> Constraint) where
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
  LoadMI :: Register -> MemoryAddress -> InstructionImpl Prelude.Monad

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

instance Bounded a => Bounded (Sym a) where
  minBound = SConst minBound
  maxBound = SConst maxBound