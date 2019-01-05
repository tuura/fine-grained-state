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
import TypedFS
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

-- | Programs are stored in program memory.
type InstructionAddress = MachineValue

-- | Binary representation of an instruction
type InstructionCode = MachineValue -- Word16
