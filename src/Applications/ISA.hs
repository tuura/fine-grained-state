{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             GADTs,
             StandaloneDeriving,
             MultiWayIf,
             LambdaCase,
             ApplicativeDo #-}

module Applications.ISA where

import Prelude hiding (Monad, abs, div, mod)
import qualified Prelude (Monad, abs, div, mod)
import Data.Functor (void)
import Data.Foldable (sequenceA_)
import Control.Selective hiding (dependencies)
import Applications.ISA.Types
import Applications.ISA.Instruction
import TypedFS

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
-- 'r' is the register type
-- 'addr' is the memory address type
-- 'iaddr' is the instruction address type
-- 'flag' is the flag type
data MachineKey a where
    Reg  :: Register -> MachineKey MachineValue
    -- ^ register
    Addr :: MemoryAddress -> MachineKey MachineValue
    -- ^ memory address
    F    :: Flag -> MachineKey Bool
    -- -- ^ flag
    IC   :: MachineKey MachineValue
    -- -- ^ instruction counter
    IR   :: MachineKey (Instruction Unconstrained)
    -- ^ instruction register
    Prog :: InstructionAddress -> MachineKey (Instruction Unconstrained)
    -- ^ program memory address

instance Key (MachineKey a) where
    type Value (MachineKey a) = a
    showKey = show

instance Show (MachineKey a) where
    show key = case key of
        Reg reg -> show reg
        Addr addr -> show addr
        F f -> show f
        IC  -> "IC"
        IR  -> "IR"
        Prog addr -> "Prog " ++ show addr

semantics :: [Instruction Applicative] -> FS Applicative ()
semantics program read write =
    sequenceA_ $ map (\i -> instruction i read write) program
    where
        instruction :: Instruction c -> FS c ()
        instruction i read write = case i of
            Halt -> haltF read write
            Load reg addr -> load reg addr read write
            LoadMI reg addr -> loadMI reg addr read write
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

-- | Halt the execution.
--   Functor.
haltF :: FS Functor ()
haltF read write = void $
    write (F Halted) ((const True) <$> read (F Halted))

-- | Halt the execution.
--   Applicative.
haltA :: FS Applicative ()
haltA read write = void $ do
    write (F Halted) (pure True)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: Register -> SImm8
     -> FS Functor ()
setF reg simm read write = void $
    write (Reg reg) ((const . fromIntegral $ simm) <$> (read (Reg reg)))

-- -- | Set a register value.
-- --   Applicative.
-- setA :: Register -> SImm8
--                        -> FS Applicative a ()
-- setA reg simm read write = Just $
--     write (Reg reg) (pure . fromIntegral $ simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> FS Functor ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress
    -> FS Applicative ()
add reg addr = \read write -> void $
    let result = (+) <$> (read (Reg reg)) <*> read (Addr addr)
    in write (F Zero) ((== 0) <$> write (Reg reg) result)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative ()
sub reg addr = \read write -> void $
    let result = (-) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((== 0) <$> write (Reg reg) result)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> FS Applicative ()
mul reg addr = \read write -> void $
    let result = (*) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((== 0) <$> write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> FS Applicative ()
div reg addr = \read write -> void $
    let result = Prelude.div <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((== 0) <$> write (Reg reg) result)

mod :: Register -> MemoryAddress -> FS Applicative ()
mod reg addr = \read write -> void $
    let result = Prelude.mod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((== 0) <$> write (Reg reg) result)

abs :: Register -> FS Functor ()
abs reg = \read write -> void $
    let result = Prelude.abs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor ()
jump simm read write = void $
    write IC (fmap ((+) . fromIntegral $ simm) (read IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: Register -> MemoryAddress -> FS Selective ()
loadMI reg addr read write = void $ do
    read (Addr addr) `bindS` \addr' ->
        write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: SImm8 -> FS Selective ()
jumpZero simm read write = void $
    ifS (read (F Zero))
        (write IC ((fromIntegral simm +) <$> read IC))
        (write IC (read IC))

--------------------------------------------------------------------------------

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- --------------------------------------------------------------------------------
-- executeInstruction :: FS Monad ()
-- executeInstruction = \read write -> do
--     -- fetch instruction
--     ic <- read IC
--     write IR (read (Prog ic))
--     -- increment instruction counter
--     write IC (pure $ ic + 1)
--     -- read instruction register and execute the instruction
--     i <- read IR
--     semantics i read write
-- executeInstruction :: FS Selective ()
-- executeInstruction = \read write ->
--     -- fetch instruction
--     read IC `bindS` \ic ->
--     write IR (read (Prog ic)) *>
--     -- increment instruction counter
--     write IC (pure $ ic + 1) *>
--     -- read instruction register and execute the instruction
--     read IR `bindS` \i -> semantics i read write