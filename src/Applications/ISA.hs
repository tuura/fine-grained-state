{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             GADTs,
             StandaloneDeriving,
             MultiWayIf,
             LambdaCase #-}

module Applications.ISA where

import Control.Selective
import Data.Bits
import Applications.ISA.Types
import Applications.ISA.Instruction
import Applications.ISA.Instruction.Decode
import Applications.ISA.Instruction.Encode
import Data.Word (Word16)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import Prelude hiding (Monad, abs, div, mod, readIO)
import qualified Prelude (Monad, abs, div, mod)
import Data.Functor (void)
import Data.Foldable (sequenceA_)
import Control.Selective hiding (dependencies)
import Applications.ISA.Types
import Applications.ISA.Instruction
import FS

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
-- 'r' is the register type
-- 'addr' is the memory address type
-- 'iaddr' is the instruction address type
-- 'flag' is the flag type
data MachineKey where
    Reg  :: Register -> MachineKey
    -- ^ register
    Addr :: MemoryAddress -> MachineKey
    -- ^ memory address
    F    :: Flag -> MachineKey
    -- -- ^ flag
    IC   :: MachineKey
    -- -- ^ instruction counter
    IR   :: MachineKey
    -- ^ instruction register
    Prog :: InstructionAddress -> MachineKey
    -- ^ program memory address

instance Show MachineKey where
    show key = case key of
        Reg reg -> show reg
        Addr addr -> show addr
        F f -> show f
        IC  -> "IC"
        IR  -> "IR"
        Prog addr -> "Prog " ++ show addr

semantics :: [Instruction] -> FS Selective MachineKey MachineValue ()
semantics program read write =
    sequenceA_ $ map (\i -> instructionSemantics i read write) program

instructionSemantics :: Instruction -> FS Selective MachineKey MachineValue ()
instructionSemantics i read write = case i of
    IF i -> semanticsFunctor i read write
    IA i -> semanticsApplicative i read write
    IS i -> semanticsSelective i read write

semanticsFunctor :: InstructionFunctor -> FS Functor MachineKey MachineValue ()
semanticsFunctor i read write = case i of
    Halt -> haltF read write
    Load reg addr -> load reg addr read write
    Set reg simm8  -> setF reg simm8 read write
    Store reg addr -> store reg addr read write
    Jump simm8     -> jump simm8 read write

semanticsApplicative :: InstructionApplicative -> FS Applicative MachineKey MachineValue ()
semanticsApplicative i read write = case i of
    Add reg addr   -> add reg addr read write
    Sub reg addr   -> sub reg addr read write
    Mul reg addr   -> mul reg addr read write
    Div reg addr   -> div reg addr read write
    Mod reg addr   -> mod reg addr read write
    Abs reg        -> abs reg read write

semanticsSelective :: InstructionSelective -> FS Selective MachineKey MachineValue ()
semanticsSelective i read write = case i of
    LoadMI reg addr -> loadMI reg addr read write
    JumpZero simm8 -> jumpZero simm8 read write

-- | Halt the execution.
--   Functor.
haltF :: FS Functor MachineKey MachineValue ()
haltF read write = void $
    write (F Halted) ((boolToMachineValue . const True) <$> read (F Halted))

-- -- | Halt the execution.
-- --   Applicative.
-- haltA :: FS Applicative ()
-- haltA read write = void $ do
--     write (F Halted) (pure True)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor MachineKey MachineValue ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: Register -> SImm8
     -> FS Functor MachineKey MachineValue ()
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
store :: Register -> MemoryAddress -> FS Functor MachineKey MachineValue ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress
    -> FS Applicative MachineKey MachineValue ()
add reg addr = \read write -> void $
    let result = (+) <$> (read (Reg reg)) <*> read (Addr addr)
    in write (F Zero) (write (Reg reg) result)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative MachineKey MachineValue ()
sub reg addr = \read write -> void $
    let result = (-) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) (write (Reg reg) result)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> FS Applicative MachineKey MachineValue ()
mul reg addr = \read write -> void $
    let result = (*) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) (write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> FS Applicative MachineKey MachineValue ()
div reg addr = \read write -> void $
    let result = Prelude.div <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) (write (Reg reg) result)

mod :: Register -> MemoryAddress -> FS Applicative MachineKey MachineValue ()
mod reg addr = \read write -> void $
    let result = Prelude.mod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) (write (Reg reg) result)

abs :: Register -> FS Functor MachineKey MachineValue ()
abs reg = \read write -> void $
    let result = Prelude.abs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor MachineKey MachineValue ()
jump simm read write = void $
    write IC (fmap ((+) . fromIntegral $ simm) (read IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: Register -> MemoryAddress -> FS Selective MachineKey MachineValue ()
loadMI reg addr read write = void $ do
    read (Addr addr) `bindS` \addr' ->
        write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: SImm8 -> FS Selective MachineKey MachineValue ()
jumpZero simm read write = void $
    ifS ((== 0) <$> read (F Zero))
        (write IC ((fromIntegral simm +) <$> read IC))
        (write IC (read IC))

--------------------------------------------------------------------------------

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- -- --------------------------------------------------------------------------------
executeInstruction :: FS Monad MachineKey MachineValue ()
executeInstruction = \read write -> do
    -- fetch instruction
    ic <- read IC
    write IR (read (Prog ic))
    -- increment instruction counter
    write IC (pure $ ic + 1)
    -- read instruction register and execute the instruction
    i <- decode <$> read IR
    instructionSemantics i read write
    -- write IR (pure . encode . IA $ Add R0 0)
    -- add R0 0 read write
