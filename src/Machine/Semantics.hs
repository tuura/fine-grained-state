module Machine.Semantics where

import Prelude hiding (Read, Monad, div, mod, abs)
import qualified Prelude (Read, Monad, div, mod, abs)
import Data.Functor (void)
import Control.Selective
import Machine.Types

--------------------------------------------------------------------------------
---------------- Instruction Semantics -----------------------------------------
--------------------------------------------------------------------------------

data Key a where
    Reg  :: Register -> Key (Value)
    -- ^ register
    Addr :: MemoryAddress -> Key (Value)
    -- ^ memory address
    F   :: Flag -> Key (Bool)
    -- -- ^ flag
    IC   :: Key (Value)
    -- -- ^ instruction counter
    IR   :: Key (InstructionCode)
    -- ^ instruction register
    Prog :: InstructionAddress -> Key (InstructionCode)
    -- ^ program memory address

instance Show (Key a) where
    show = \case
        Reg  reg   -> show reg
        Addr addr  -> show addr
        F    flag  -> show flag
        IC         -> "IC"
        IR         -> "IR"
        Prog addr  -> show addr

type Read f = forall a. Key a -> f (Sym a)

type Write f = forall a. Key a -> f (Sym a) -> f (Sym a)

type FS c a = forall f. c f => Read f ->
                               Write f ->
                               f a


-- | Halt the execution.
--   Applicative.
halt :: FS Applicative ()
halt read write = void $
    write (F Halted) (pure (SConst True))

set :: Register -> SImm8 -> FS Applicative ()
set reg simm read write = void $
    write (Reg reg) (pure (SConst . fromIntegral $ simm))

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress
    -> FS Applicative ()
add reg addr = \read write -> void $
    let result = SAdd <$> read (Reg reg) <*> read (Addr addr)
    in write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> FS Functor ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative ()
sub reg addr = \read write -> void $
    let result = (SSub) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> FS Applicative ()
mul reg addr = \read write -> void $
    let result = (SMul) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> FS Applicative ()
div reg addr = \read write -> void $
    let result = SDiv <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

mod :: Register -> MemoryAddress -> FS Applicative ()
mod reg addr = \read write -> void $
    let result = SMod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

abs :: Register -> FS Functor ()
abs reg = \read write -> void $
    let result = SAbs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor ()
jump simm read write = void $
    write IC (fmap (SAdd (SConst . fromIntegral $ simm)) (read IC))

jumpZero :: SImm8 -> FS Selective ()
jumpZero _ _ _ = error "jumpZero not implemented"

instructionSemantics :: Instruction -> FS Selective ()
instructionSemantics (Instruction i) r w = case i of
    Halt -> halt r w
    Load reg addr -> load reg addr r w
    -- -- LoadMI reg addr -> loadMI reg addr r w
    Set reg simm8  -> set reg simm8 r w
    Store reg addr -> store reg addr r w
    Add reg addr   -> add reg addr r w
    Sub reg addr   -> sub reg addr r w
    Mul reg addr   -> mul reg addr r w
    Div reg addr   -> div reg addr r w
    Mod reg addr   -> mod reg addr r w
    Abs reg        -> abs reg r w
    Jump simm8     -> jump simm8 r w
    JumpZero simm8 -> jumpZero simm8 r w