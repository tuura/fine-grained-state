{-# LANGUAGE GeneralizedNewtypeDeriving,
             ConstraintKinds,
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
import Data.Bits
import Control.Selective
import FS
import Applications.ISA.Types

class Unconstrained (a :: * -> *)
instance Unconstrained a

data InstructionFunctor where
  Halt :: InstructionFunctor
  Load   :: Register -> MemoryAddress -> InstructionFunctor
  Set    :: Register -> SImm8 -> InstructionFunctor
  Store  :: Register -> MemoryAddress -> InstructionFunctor
  Jump :: SImm8 -> InstructionFunctor

data InstructionApplicative where
  Add    :: Register -> MemoryAddress -> InstructionApplicative
  Sub    :: Register -> MemoryAddress -> InstructionApplicative
  Mul    :: Register -> MemoryAddress -> InstructionApplicative
  Div    :: Register -> MemoryAddress -> InstructionApplicative
  Mod    :: Register -> MemoryAddress -> InstructionApplicative
  Abs    :: Register -> InstructionApplicative

data InstructionSelective where
  JumpZero :: SImm8 -> InstructionSelective
  LoadMI :: Register -> MemoryAddress -> InstructionSelective

deriving instance Show InstructionFunctor
deriving instance Read InstructionFunctor
deriving instance Show InstructionApplicative
deriving instance Read InstructionApplicative
deriving instance Show InstructionSelective
deriving instance Read InstructionSelective

data Instruction = IF InstructionFunctor
                 | IA InstructionApplicative
                 | IS InstructionSelective
  deriving (Show, Read)

-- | Programs are stored in program memory.
type InstructionAddress = MachineValue

-- -- | Binary representation of an instruction
type InstructionCode = MachineValue
-- newtype InstructionCode (c :: (* -> *) -> Constraint) =
--   InstructionCode { getInstructioCode :: MachineValue } deriving (Show, Eq, Num, Bits, FiniteBits)
