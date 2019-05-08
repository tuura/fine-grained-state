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
--
--   It looks like we after all need a Monad here since Applicative theoretically permits reordering
--   of effects. Here, it is important for the overflow check to happen before the addition, otherwise
--   the check may be executed with the updated value of the register and will give an invalid result.
add :: Register -> MemoryAddress
    -> FS Applicative ()
add reg addr = \read write -> void $
    let arg1     = read (Reg reg)
        arg2     = read (Addr addr)
        result   = SAdd <$> arg1
                        <*> arg2
        overflow = willOverflow <$> arg1 <*> arg2
    in write (F Overflow) overflow *>
       write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    where willOverflow :: Sym Value -> Sym Value -> Sym Bool
          willOverflow x y =
              let o1 = SGt y (SConst 0)
                  o2 = SGt x (SSub maxBound y)
                  o3 = SLt y (SConst 0)
                  o4 = SLt x (SSub minBound y)
              in  SOr (SAnd o1 o2)
                      (SAnd o3 o4)
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

    -- let overflow = arg2 .> 0 &&& arg1 .< (minBound @Value + arg2) |||
    --                arg2 .< 0 &&& arg1 .> (maxBound @Value + arg2)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative ()
sub reg addr = \read write -> void $
    let arg1     = read (Reg reg)
        arg2     = read (Addr addr)
        result   = SSub <$> arg1
                        <*> arg2
        overflow = willOverflow <$> arg1 <*> arg2
    in write (F Overflow) overflow *>
       write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    where willOverflow :: Sym Value -> Sym Value -> Sym Bool
          willOverflow x y =
              let o1 = SGt y (SConst 0)
                  o2 = SLt x (SAdd minBound y)
                  o3 = SLt y (SConst 0)
                  o4 = SGt x (SAdd maxBound y)
              in  SOr (SAnd o1 o2)
                      (SAnd o3 o4)

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
    let arg1     = read (Reg reg)
        arg2     = read (Addr addr)
        result   = SMod <$> arg1
                        <*> arg2
        overflow = willOverflow <$> arg1 <*> arg2
    in write (F Overflow) overflow *>
       write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    where willOverflow :: Sym Value -> Sym Value -> Sym Bool
          willOverflow x y =
              let o1 = SEq y (SConst 0)
                  o2 = SEq x minBound
                  o3 = SEq y (SConst (-1))
              in  SOr o1
                      (SAnd o2 o3)

abs :: Register -> FS Applicative ()
abs reg = \read write -> void $
    let result = SAbs <$> read (Reg reg)
        overflow x = SLt <$> x <*> pure (SConst 0)
    in  write (F Overflow) (overflow $ write (Reg reg) result)

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