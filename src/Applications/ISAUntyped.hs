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
             ApplicativeDo #-}

module Applications.ISAUntyped where

import Prelude hiding (Monad, abs, div, mod, readIO)
import qualified Prelude (Monad, abs, div, mod)
import Data.Functor (void)
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

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
-- 'r' is the register type
-- 'addr' is the memory address type
-- 'iaddr' is the instruction address type
-- 'flag' is the flag type
data MachineKey a where
    Reg  :: Register -> MachineKey (Sym Value)
    -- ^ register
    Addr :: MemoryAddress -> MachineKey (Sym Value)
    -- ^ memory address
    F    :: Flag -> MachineKey (Sym Bool)
    -- -- ^ flag
    IC   :: MachineKey Value
    -- -- ^ instruction counter
    IR   :: MachineKey InstructionCode
    -- ^ instruction register
    Prog :: InstructionAddress -> MachineKey InstructionCode
    -- ^ program memory address

deriving instance Eq a  => Eq (MachineKey a)
deriving instance Ord a => Ord (MachineKey a)

instance Show (MachineKey a) where
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

instructionSemantics :: InstructionImpl c -> FS c MachineKey ()
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
    -- JumpZero simm8 -> jumpZero simm8 read write

instructionSemantics' :: Instruction -> FS Selective MachineKey ()
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
    -- JumpZero simm8 -> jumpZero simm8 read write

-- -- | Halt the execution.
-- --   Functor.
-- -- haltF :: FS Functor MachineKey ()
-- -- haltF read write = void $
-- --     -- read (F Halted)
-- --     write (F Halted) ((const 1) <$> read (F Halted))

-- | Halt the execution.
--   Applicative.
halt :: FS Applicative MachineKey ()
halt read write = void $ do
    write (F Halted) (pure (SConst True))

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor MachineKey ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: Register -> SImm8
     -> FS Functor MachineKey ()
setF reg simm read write = void $
    write (Reg reg) ((const . SConst . fromIntegral $ simm) <$> (read (Reg reg)))

-- | Set a register value.
--   Applicative.
setA :: Register -> SImm8 -> FS Applicative MachineKey ()
setA reg simm read write = void  $
    write (Reg reg) (pure . SConst . fromIntegral $ simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> FS Functor MachineKey ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress
    -> FS Applicative MachineKey ()
add reg addr = \read write -> void $
    let result = SAdd <$> (read (Reg reg)) <*> read (Addr addr)
    in write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in write (F Zero) (write (Reg reg) result)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative MachineKey ()
sub reg addr = \read write -> void $
    let result = SSub <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> FS Applicative MachineKey ()
mul reg addr = \read write -> void $
    let result = SMul <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> FS Applicative MachineKey ()
div reg addr = \read write -> void $
    let result = SDiv <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

mod :: Register -> MemoryAddress -> FS Applicative MachineKey ()
mod reg addr = \read write -> void $
    let result = SMod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

abs :: Register -> FS Functor MachineKey ()
abs reg = \read write -> void $
    let result = SAbs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor MachineKey ()
jump simm read write = void $
    write IC (fmap ((+) . fromIntegral $ simm) (read IC))

-- -- | Indirect memory access.
-- --   Monadic.
-- loadMI :: Register -> MemoryAddress -> FS Monad MachineKey ()
-- loadMI reg addr read write =
--     void $
--     read (Addr addr) >>= \addr' ->
--         write (Reg reg) (read (Addr addr'))

-- -- | Jump if 'Zero' flag is set.
-- --   Selective.
-- jumpZero :: SImm8
--          -> FS Selective MachineKey ()
-- jumpZero simm = \read write ->
--     whenS (read (F Zero))
--           (void $ write IC ((fromIntegral simm +) <$> read IC))
--     -- ifS ((/=) <$> read (F Zero) <*> pure 0)
--     --     (void $ write IC ((fromIntegral simm +) <$> read IC))
--     --     (pure ())
-- --------------------------------------------------------------------------------
-- -- executeInstruction :: FS Monad MachineKey ()
-- -- executeInstruction = \read write -> do
-- --     -- fetch instruction
-- --     ic <- read IC
-- --     write IR (read (Prog ic))
-- --     -- increment instruction counter
-- --     write IC (pure $ ic + 1)
-- --     -- read instruction register and execute the instruction
-- --     i <- read IR
-- --     instructionSemantics' i read write
