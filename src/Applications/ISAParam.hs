{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             GADTs,
             StandaloneDeriving,
             DerivingVia,
             MultiWayIf,
             LambdaCase,
             MultiParamTypeClasses #-}

module Applications.ISAParam where

import Prelude hiding (Monad, abs, div, mod, readIO)
import qualified Prelude (Monad, abs, div, mod)
import Data.Functor (void)
import Control.Monad.Identity
import Control.Applicative (liftA2)
import Control.Comonad
import Data.Foldable (sequenceA_)
import Control.Selective hiding (dependencies)
import Applications.ISA.Types
import Applications.ISA.Symbolic.Types
import Applications.ISA.Instruction
import FS

-- type FS c k v a = forall f. c f => (k -> f v) ->
--                                    (k -> f v -> f ()) ->
--                                    f a

newtype Concrete a = Concrete {unConcrete :: a }
    deriving (Eq, Show)
    deriving (Functor, Applicative, Selective, Prelude.Monad, Comonad) via Identity

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
-- 'r' is the register type
-- 'addr' is the memory address type
-- 'iaddr' is the instruction address type
-- 'flag' is the flag type
data MachineKey f a where
    Reg  :: Register -> MachineKey f (f Value)
    -- ^ register
    Addr :: MemoryAddress -> MachineKey f (f Value)
    -- ^ memory address
    F    :: Flag -> MachineKey f (f Bool)
    -- -- ^ flag
    IC   :: MachineKey f (f Value)
    -- -- ^ instruction counter
    IR   :: MachineKey f (f InstructionCode)
    -- ^ instruction register
    Prog :: InstructionAddress -> MachineKey f (f InstructionCode)
    -- ^ program memory address

deriving instance Eq a  => Eq (MachineKey f a)
deriving instance Ord a => Ord (MachineKey f a)

instance Show (MachineKey f a) where
    show key = case key of
        Reg reg -> show reg
        Addr addr -> show addr
        F f -> show f
        IC  -> "IC"
        IR  -> "IR"
        Prog addr -> "Prog " ++ show addr

-- semantics :: [InstructionImpl Applicative] -> FS Applicative MachineKey ()
-- semantics instrs read write =
--     sequenceA_ $ map (\i -> instructionSemantics i read write) instrs

-- semantics' :: [Instruction] -> FS Selective MachineKey ()
-- semantics' instrs read write =
--     sequenceA_ $ map (\i -> instructionSemantics' i read write) instrs

instructionSemantics :: (Interpretable f, Comonad f) => InstructionImpl c -> FS c (MachineKey f) ()
instructionSemantics i read write = case i of
    Halt -> halt read write
    Load reg addr -> load reg addr read write
    -- LoadMI reg addr -> loadMI reg addr read write
    Set reg simm8  -> setF reg simm8 read write
    Store reg addr -> store reg addr read write
    Add reg addr   -> add reg addr read write
    Sub reg addr   -> sub reg addr read write
    Mul reg addr   -> mul reg addr read write
    Div reg addr   -> div reg addr read write
    Mod reg addr   -> mod reg addr read write
    Abs reg        -> abs reg read write
    Jump simm8     -> jump simm8 read write
    JumpZero simm8 -> jumpZero simm8 read write

instructionSemantics' :: (Interpretable f, Comonad f) => Instruction -> FS Selective (MachineKey f) ()
instructionSemantics' (Instruction i) read write = case i of
    Halt -> halt read write
    Load reg addr -> load reg addr read write
    -- LoadMI reg addr -> loadMI reg addr read write
    Set reg simm8  -> setF reg simm8 read write
    Store reg addr -> store reg addr read write
    Add reg addr   -> add reg addr read write
    Sub reg addr   -> sub reg addr read write
    Mul reg addr   -> mul reg addr read write
    Div reg addr   -> div reg addr read write
    Mod reg addr   -> mod reg addr read write
    Abs reg        -> abs reg read write
    Jump simm8     -> jump simm8 read write
    JumpZero simm8 -> jumpZero simm8 read write

-- -- -- | Halt the execution.
-- -- --   Functor.
-- -- -- haltF :: FS Functor MachineKey ()
-- -- -- haltF read write = void $
-- -- --     -- read (F Halted)
-- -- --     write (F Halted) ((const 1) <$> read (F Halted))

class Interpretable f where
    sConst :: a -> f a
    sAdd   :: f Value -> f Value -> f Value
    sSub   :: f Value -> f Value -> f Value
    sMul   :: f Value -> f Value -> f Value
    sDiv   :: f Value -> f Value -> f Value
    sMod   :: f Value -> f Value -> f Value
    sAbs   :: f Value -> f Value
    sEq    :: f Value -> f Value -> f Bool
    sGt    :: f Value -> f Value -> f Bool
    sLt    :: f Value -> f Value -> f Bool
    sAnd   :: f Bool -> f Bool -> f Bool
    sOr    :: f Bool -> f Bool -> f Bool
    sNot   :: f Bool -> f Bool

instance Interpretable Concrete where
  sConst = pure
  sAdd = liftA2 (+)
  sSub = liftA2 (-)
  sMul = liftA2 (+)
  sDiv = liftA2 (Prelude.div)
  sMod = liftA2 (Prelude.mod)
  sAbs = fmap (Prelude.abs)
  sEq  = liftA2 (==)
  sGt  = liftA2 (>)
  sLt  = liftA2 (<)
  sAnd = liftA2 (&&)
  sOr  = liftA2 (||)
  sNot = fmap not

instance Interpretable Sym where
    sConst = SConst
    sAdd = SAdd
    sSub = SSub
    sMul = SMul
    sDiv = SDiv
    sMod = SMod
    sAbs = SAbs
    sEq = SEq
    sGt = SGt
    sLt = SLt
    sAnd = SAnd
    sOr = SOr
    sNot = SNot

-- | Halt the execution.
--   Applicative.
halt :: Interpretable f =>
        FS Applicative (MachineKey f) ()
halt read write = void $ do
    write (F Halted) (pure (sConst True))

-- | Load a value from a memory location to a register.
--   Functor.
load :: Interpretable f =>
        Register -> MemoryAddress
     -> FS Functor (MachineKey f) ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: Interpretable f =>
        Register -> SImm8
     -> FS Functor (MachineKey f) ()
setF reg simm read write = void $
    write (Reg reg) ((const . sConst . fromIntegral $ simm) <$> (read (Reg reg)))

-- | Set a register value.
--   Applicative.
setA :: Interpretable f =>
        Register -> SImm8 -> FS Applicative (MachineKey f) ()
setA reg simm read write = void  $
    write (Reg reg) (pure . sConst . fromIntegral $ simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Interpretable f =>
        Register -> MemoryAddress -> FS Functor (MachineKey f) ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Interpretable f =>
        Register -> MemoryAddress
    -> FS Applicative (MachineKey f) ()
add reg addr = \read write -> void $
    let result = sAdd <$> (read (Reg reg)) <*> read (Addr addr)
    in write (F Zero) ((sEq (sConst 0)) <$> write (Reg reg) result)
    -- in write (F Zero) (write (Reg reg) result)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Interpretable f =>
        Register -> MemoryAddress -> FS Applicative (MachineKey f) ()
sub reg addr = \read write -> void $
    let result = sSub <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((sEq (sConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Interpretable f =>
        Register -> MemoryAddress -> FS Applicative (MachineKey f) ()
mul reg addr = \read write -> void $
    let result = sMul <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((sEq (sConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Interpretable f =>
        Register -> MemoryAddress -> FS Applicative (MachineKey f) ()
div reg addr = \read write -> void $
    let result = sDiv <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((sEq (sConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

mod :: Interpretable f =>
        Register -> MemoryAddress -> FS Applicative (MachineKey f) ()
mod reg addr = \read write -> void $
    let result = sMod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((sEq (sConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

abs :: Interpretable f =>
        Register -> FS Functor (MachineKey f) ()
abs reg = \read write -> void $
    let result = sAbs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: Interpretable f =>
        SImm8 -> FS Applicative (MachineKey f) ()
jump simm read write = void $
    write IC (sAdd <$> (pure $ sConst (fromIntegral $ simm)) <*> read IC)

-- | Indirect memory access.
--   Monadic.
loadMI :: (Interpretable f, Comonad f) =>
          Register -> MemoryAddress -> FS FS.Monad (MachineKey f) ()
loadMI reg addr read write = void $ do
    addr' <- extract <$> read (Addr addr)
    write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: (Interpretable f, Comonad f) =>
            SImm8 -> FS Selective (MachineKey f) ()
jumpZero simm = \read write ->
    let cond = extract <$> read (F Zero)
    in  whenS cond (void $ write IC (sAdd <$> (pure $ sConst (fromIntegral simm)) <*> read IC))
    -- let cond = extract <$> read (F Zero)
    -- in  whenS cond (void $ write IC ((fromIntegral simm +) <$> read IC))
-- -- --------------------------------------------------------------------------------
-- -- -- executeInstruction :: FS Monad MachineKey ()
-- -- -- executeInstruction = \read write -> do
-- -- --     -- fetch instruction
-- -- --     ic <- read IC
-- -- --     write IR (read (Prog ic))
-- -- --     -- increment instruction counter
-- -- --     write IC (pure $ ic + 1)
-- -- --     -- read instruction register and execute the instruction
-- -- --     i <- read IR
-- -- --     instructionSemantics' i read write
