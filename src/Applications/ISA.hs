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

import Data.Functor (void)
import Applications.ISA.Types
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
    -- IC   :: MachineKey ()
    -- -- ^ instruction counter
    -- IR   :: MachineKey ()
    -- -- ^ instruction register
    -- Prog :: InstructionAddress -> MachineKey ()
    -- -- ^ program memory address

instance Key (MachineKey a) where
    type Value (MachineKey a) = a
    showKey = show

instance Show (MachineKey a) where
    show key = case key of
        Reg reg -> show reg
        Addr addr -> show addr
        F f -> show f

-- | Halt the execution.
--   Functor.
haltF :: FS Functor ()
haltF read write = Just . void $
    write (F Halted) ((const True) <$> read (F Halted))

-- -- | Halt the execution.
-- --   Applicative.
-- haltA :: MachineValue a => Semantics Applicative MachineKey a ()
-- haltA read write = Just $
--     write (F Halted) (pure 1)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor ()
load reg addr read write = Just . void $
    write (Reg reg) (read (Addr addr))

-- -- | Set a register value.
-- --   Functor.
-- setF :: MachineValue a => Register -> SImm8
--                        -> Semantics Functor MachineKey a ()
-- setF reg simm read write = Just $
--     write (Reg reg) ((const . unsafeFromSImm8 $ simm) <$> (read (Reg reg)))

-- -- | Set a register value.
-- --   Applicative.
-- setA :: MachineValue a => Register -> SImm8
--                        -> Semantics Applicative MachineKey a ()
-- setA reg simm read write = Just $
--     write (Reg reg) (pure . unsafeFromSImm8 $ simm)

-- -- | Store a value from a register to a memory location.
-- --   Functor.
-- store :: MachineValue a => Register -> MemoryAddress
--                         -> Semantics Functor MachineKey a ()
-- store reg addr read write = Just $
--     write (Addr addr) (read (Reg reg) )

-- -- | Add a value from memory location to one in a register.
-- --   Applicative.
-- add :: MachineValue a => Register -> MemoryAddress
--                       -> Semantics Applicative MachineKey a ()
-- add reg addr = \read write -> Just $
--     let result = (+) <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result


-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress
    -> FS Applicative ()
add reg addr = \read write -> Just . void $
    let result = (+) <$> (read (Reg reg)) <*> read (Addr addr)
    in write (F Zero) ((== 0) <$> write (Reg reg) result)
        -- write2 (F Zero) (Reg reg) result

-- -- | Add a value from memory location to one in a register. Tracks overflow.
-- --   Selective.
-- addS :: MachineValue a => Register -> MemoryAddress -> Semantics Selective MachineKey a ()
-- addS reg addr = \read write -> Just $
--     let arg1   = read (Reg reg)
--         arg2   = read (Addr addr)
--         result = (+) <$> read (Reg reg) <*> read (Addr addr)
--         o1 = gt <$> arg2 <*> pure 0
--         o2 = gt <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
--         o3 = lt <$> arg2 <*> pure 0
--         o4 = lt <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
--         o  = or <$> (and <$> o1 <*> o2)
--                 <*> (and <$> o3 <*> o4)
--     in  write (F Overflow) o *>
--         write (Reg reg) result

-- -- | Sub a value from memory location to one in a register.
-- --   Applicative.
-- sub :: MachineValue a => Register -> MemoryAddress
--                       -> Semantics Applicative MachineKey a ()
-- sub reg addr = \read write -> Just $
--     let result = (-) <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result

-- -- | Multiply a value from memory location to one in a register.
-- --   Applicative.
-- mul :: MachineValue a => Register -> MemoryAddress
--                       -> Semantics Applicative MachineKey a ()
-- mul reg addr = \read write -> Just $
--     let result = (*) <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result

-- -- | Subtract a value from memory location to one in a register.
-- --   Applicative.
-- div :: MachineValue a => Register -> MemoryAddress
--                       -> Semantics Applicative MachineKey a ()
-- div reg addr = \read write -> Just $
--     let result = Value.div <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result

-- mod :: MachineValue a => Register -> MemoryAddress
--                       -> Semantics Applicative MachineKey a ()
-- mod reg addr = \read write -> Just $
--     let result = Value.mod <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result

-- abs :: MachineValue a => Register -> Semantics Functor MachineKey a ()
-- abs reg = \read write -> Just $
--     let result = Prelude.abs <$> read (Reg reg)
--     in  write (Reg reg) result

-- -- | Unconditional jump.
-- --   Functor.
-- jump :: MachineValue a => SImm8 -> Semantics Functor MachineKey a ()
-- jump simm read write = Just $
--     write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC))

-- -- | Indirect memory access.
-- --   Monadic.
-- loadMI :: MachineValue a => Register -> MemoryAddress
--                          -> Semantics Monad MachineKey a ()
-- loadMI reg addr read write = undefined
--     -- Just $ do
--     -- addr' <- read (Addr addr)
--     -- write (Reg reg) (read (Addr addr'))

-- -- | Jump if 'Zero' flag is set.
-- --   Selective.
-- jumpZero :: MachineValue a => SImm8 -> Semantics Selective MachineKey a ()
-- jumpZero simm read write = Just $
--     ifS (unsafeToBool <$> (eq <$> read (F Zero) <*> pure 0))
--         (write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC)))
--         (write IC $ read IC)
-- --------------------------------------------------------------------------------
-- executeInstruction :: Semantics Monad MachineKey Value ()
-- executeInstruction = \read write -> Just $ do
--     -- fetch instruction
--     ic <- read IC
--     write IR (read (Prog ic))
--     -- increment instruction counter
--     write IC (pure $ ic + 1)
--     -- read instruction register and execute the instruction
--     i <- read IR
--     fromJust $ semanticsM (decode i) read write