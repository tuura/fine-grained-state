{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             LambdaCase,
             TypeFamilies,
             StandaloneDeriving #-}

module Applications.ISA.Instruction where

import GHC.Exts (Constraint)
import Data.Functor (void)
import Control.Selective
import FS
import Applications.ISA.Types

class Unconstrained (a :: * -> *)
instance Unconstrained a

data InstructionImpl c where
  Halt   :: InstructionImpl Functor
  Load   :: Register -> MemoryAddress -> InstructionImpl Functor
  Set    :: Register -> SImm8 -> InstructionImpl Functor
  Store  :: Register -> MemoryAddress -> InstructionImpl Functor
  Add    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Sub    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Mul    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Div    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Mod    :: Register -> MemoryAddress -> InstructionImpl Applicative
  Abs    :: Register -> InstructionImpl Applicative
  Jump :: SImm8 -> InstructionImpl Functor
  JumpZero :: SImm8 -> InstructionImpl Selective
  LoadMI :: Register -> MemoryAddress -> InstructionImpl Selective

instance Show (InstructionImpl c) where
  show = \case
    Halt          -> "Halt"
    Load  reg addr -> "Load " ++ show reg ++ " " ++ show addr
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

deriving instance Show Instruction

-- | Programs are stored in program memory.
type InstructionAddress = MachineValue

-- | Binary representation of an instruction
type InstructionCode = MachineValue -- Word16
