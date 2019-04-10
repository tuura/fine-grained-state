{-# LANGUAGE LambdaCase, GADTs, TypeFamilies
            , RankNTypes, MultiParamTypeClasses
            , DeriveFunctor, StandaloneDeriving, ScopedTypeVariables #-}

module Applications.ISA.Symbolic.Types where

import qualified Data.Tree as Tree
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
-- import Text.Pretty.Simple
import Control.Monad.State
import Control.Monad.List
import Applications.ISA.Types
import Applications.ISA.Instruction
import Applications.ISA.Instruction.Decode
import Applications.ISA.Instruction.Encode
-- import Machine.Instruction.Encode
import Applications.ISA.Program hiding (readProgram)
import Data.Word (Word8)
import Data.Int (Int16)

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

-- | The state of symbolic computation
data SymState = SymState { registers         :: Map.Map Register (Sym Value)
                         , instructionCounter :: InstructionAddress -- Sym
                         , instructionRegister :: InstructionCode
                         , flags :: Map.Map Flag (Sym Bool)
                         , memory :: Map.Map Word8 (Sym Value)
                         , program :: Program
                         , clock :: Clock

                         , pathConstraintList :: [Sym Bool]
                         }

renderSymState :: SymState -> String
renderSymState state =
  "IC: " <> show (instructionCounter state) <> "\n" <>
  "IR: " <> show (decode $ instructionRegister state) <> "\n" <>
  "Flags: " <> show (Map.toList $ flags state) <> "\n" <>
  "Path Constraints: \n" <> renderPathConstraints (pathConstraintList state) <> "\n"
  where
    renderPathConstraints :: Show a => [Sym a] -> String
    renderPathConstraints xs = foldr (\x acc -> " && " <> show x <> "\n" <> acc) "" xs

-- -- instance Show SymState where
-- --     show state = unlines [ "IC: " <> show (instructionCounter state)
-- --                          , "IR: " <> show (decode $ instructionRegister state)
-- --                          , "Registers: " <> show (Map.toList $ registers state)
-- --                          , "Flags: " <> show (Map.toList $ flags state)
-- --                          , "Constraints: " <> show (pathConstraintList state)
-- --                          ]

instance Show SymState where
    show state = show (instructionCounter state)

-- newtype Computation a = Computation { unComputation :: State SymState a }
-- -- newtype Computation a = Computation { unComputation :: State SymState [a] }

-- runComputation :: Computation a -> SymState -> (a, SymState)
-- runComputation (Computation c) initState = runState c initState

-- | The symbolic execution trace
type Trace = Tree.Tree SymState

-- type GTrace = G.Graph SymState

emptyRegisters :: Map.Map Register (Sym Value)
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] (map SConst [0, 0..])

emptyFlags :: Map.Map Flag (Sym Bool)
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map SConst $ repeat False)

initialiseMemory :: [(MemoryAddress, Sym Value)] -> Map.Map Word8 (Sym Value)
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
    in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

boot :: Program -> Map.Map Word8 (Sym Value) -> SymState
boot prog mem = SymState { registers = emptyRegisters
                         , instructionCounter = 0
                         , instructionRegister = encode . Instruction $ Jump 0
                         , program = prog
                         , flags = emptyFlags
                         , memory = mem
                         , clock = 0

                         , pathConstraintList = []
                         }
