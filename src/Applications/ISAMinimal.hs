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

module Applications.ISAMinimal where

import Prelude hiding (Monad, abs, div, mod, readIO)
import qualified Prelude (Monad, abs, div, mod)
import Data.Int (Int8)
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.Functor (void)
import Data.Functor.Identity
import Data.Foldable (sequenceA_)
import Control.Selective hiding (dependencies)
import Applications.ISA.Types
import Applications.ISA.Program
import Applications.ISA.Instruction
import Applications.ISA.Instruction.Decode
import FS
import Data.SBV as SBV

type Value = Int8

type VarId = Word8

data Expr a where
    SConst :: Value -> Expr Value
    SVar   :: VarID -> Expr Value
    SEq    :: Expr a -> Expr a -> Expr Bool
    SNot   :: Expr Bool -> Expr Bool
    
    -- SAdd Expr Expr
    -- SAnd Expr Expr


instance Show Expr where
    show (SConst x)   = show x
    show (SVar n  )   = "var_" <> show n
    show (SEq  x y)   = "(" <> show x <> " == " <> show y <> ")"
    show (SAdd x y)   = "(" <> show x <> " + " <> show y <> ")"
    show (SAnd x y)   = "(" <> show x <> " & " <> show y <> ")"
    show (SNot b )    = "¬" <> show b

-- -- | The state of symbolic computation
-- data SymState = SymState { registers         :: Map.Map Register Expr
--                          , instructionCounter :: InstructionAddress -- Expr
--                          , instructionRegister :: InstructionCode
--                          , flags :: Map.Map Flag Expr
--                          , memory :: Map.Map Word8 Expr
--                          , program :: Program
--                          , clock :: Clock

--                          , pathConstraintList :: [Expr]
--                          }

-- instance Show SymState where
--     show state = unlines [ "IC: " <> show (instructionCounter state)
--                          , "IR: " <> show (decode $ instructionRegister state)
--                          , "Registers: " <> show (Map.toList $ registers state)
--                          , "Flags: " <> show (Map.toList $ flags state)
--                          , "Constraints: " <> show (pathConstraintList state)
--                          ]

-- emptyRegisters :: Map.Map Register Expr
-- emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] (map SConst [0, 0..])

-- emptyFlags :: Map.Map Flag Expr
-- emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map SConst [0, 0..])

-- initialiseMemory :: [(MemoryAddress, Expr)] -> Map.Map Word8 Expr
-- initialiseMemory vars =
--     let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
--     in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

-- boot :: Program -> Map.Map Word8 Expr -> SymState
-- boot prog mem = SymState { registers = emptyRegisters
--                          , instructionCounter = 0
--                          , instructionRegister = 0 -- encode $ Jump 0
--                          , program = prog
--                          , flags = emptyFlags
--                          , memory = mem
--                          , clock = 0

--                          , pathConstraintList = []
--                          }

-- ----------------------------------------------------------------------------------------------------
-- --------------- Semantics --------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------------------

-- data MachineKey a where
--     Reg  :: Register -> MachineKey Expr
--     -- ^ register
--     Addr :: MemoryAddress -> MachineKey Expr
--     -- ^ memory address
--     F    :: Flag -> MachineKey Expr
--     -- -- ^ flag
--     IC   :: MachineKey Expr
--     -- -- ^ instruction counter
--     IR   :: MachineKey Instruction
--     -- ^ instruction register
--     Prog :: InstructionAddress -> MachineKey Instruction
--     -- ^ program memory address

-- deriving instance Eq a  => Eq (MachineKey a)
-- deriving instance Ord a => Ord (MachineKey a)

-- instance Show (MachineKey a) where
--     show key = case key of
--         Reg reg -> show reg
--         Addr addr -> show addr
--         F f -> show f
--         IC  -> "IC"
--         IR  -> "IR"
--         Prog addr -> "Prog " ++ show addr

-- -- | Halt the execution.
-- --   Applicative.
-- halt :: FS Applicative MachineKey ()
-- halt read write = void $ do
--     write (F Halted) (pure (SConst 1))

-- -- | Load a value from a memory location to a register.
-- --   Functor.
-- load :: Register -> MemoryAddress
--      -> FS Functor MachineKey ()
-- load reg addr read write = void $
--     write (Reg reg) (read (Addr addr))

-- -- | Add a value from memory location to one in a register.
-- --   Applicative.
-- add :: Register -> MemoryAddress
--     -> FS Applicative MachineKey ()
-- add reg addr = \read write -> void $
--     let result = SAdd <$> (read (Reg reg)) <*> read (Addr addr)
--     in write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)

-- -- | Unconditional jump.
-- --   Functor.
-- jump :: SImm8 -> FS Functor MachineKey ()
-- jump simm read write = void $
--     write IC (fmap (SAdd . SConst $ simm) (read IC))

-- -- | Jump if 'Zero' flag is set.
-- --   Selective.
-- jumpZero :: SImm8
--          -> FS Selective MachineKey ()
-- jumpZero simm = \read write ->
--     -- whenS (read (F Zero))
--     --       (void $ write IC ((fromIntegral simm +) <$> read IC))
--     ifS ((/=) <$> read (F Zero) <*> SConst 0)
--         (void $ write IC ((fromIntegral simm +) <$> read IC))
--         (pure ())