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

data Instruction c where
  Halt   :: Instruction Functor
  Load   :: Register -> MemoryAddress -> Instruction Functor
  Set    :: Register -> SImm8 -> Instruction Functor
  Store  :: Register -> MemoryAddress -> Instruction Functor
  Add    :: Register -> MemoryAddress -> Instruction Applicative
  Sub    :: Register -> MemoryAddress -> Instruction Applicative
  Mul    :: Register -> MemoryAddress -> Instruction Applicative
  Div    :: Register -> MemoryAddress -> Instruction Applicative
  Mod    :: Register -> MemoryAddress -> Instruction Applicative
  Abs    :: Register -> Instruction Applicative
  Jump :: SImm8 -> Instruction Functor
  JumpZero :: SImm8 -> Instruction Selective
  LoadMI :: Register -> MemoryAddress -> Instruction Selective

instance Show (Instruction c) where
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

-- | Programs are stored in program memory.
type InstructionAddress = MachineValue

-- | Binary representation of an instruction
type InstructionCode = MachineValue -- Word16
