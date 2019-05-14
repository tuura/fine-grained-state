-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Assembly
-- Copyright   :  (c) Georgy Lukyanov 2019
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- An shallowly-embedded assembly language.
--
-----------------------------------------------------------------------------
module Machine.Assembly where

import Data.Int (Int8)
import Machine.Types
import Machine.Encode
import Machine.Decode
import Control.Monad (ap)

type P = [Instruction]

-- | An assembly writer monad.
data Writer a = Writer
    { runWriter :: P -> (a, P)
    } deriving Functor

instance Applicative Writer where
    pure  = return
    (<*>) = ap

instance Prelude.Monad Writer where
    return a         = Writer (\p -> (a, p))
    Writer w >>= f = Writer (\p -> let (a, p') = w p in runWriter (f a) p')

newtype Label = Label Int8

label :: Writer Label
label = Writer (\p -> (Label (fromIntegral $ length p), p))

goto :: Label -> Script
goto (Label there) = do
    Label here <- label
    let offset = fromIntegral (there - here - 1)
    jmpi offset -- TODO: Add error handling if offset is too large

instance Show a => Show (Writer a) where
    show s = show $ reverse $ snd $ runWriter s []

type Script = Writer ()

assemble :: Script -> Program
assemble s = zip [0..] (map encode prg)
  where
    prg = reverse $ snd $ runWriter s []

tell :: Instruction -> Script
tell i = Writer (\p -> ((), i:p))

-- Instructions
-- and   rX dmemaddr = tell (Instruction $ And rX dmemaddr)
-- or    rX dmemaddr = tell (Instruction $ Or  rX dmemaddr)
-- xor    rX dmemaddr = tell (Instruction $ Xor  rX dmemaddr)

add   rX dmemaddr = tell (Instruction $ Add    rX dmemaddr)
sub   rX dmemaddr = tell (Instruction $ Sub    rX dmemaddr)
mul   rX dmemaddr = tell (Instruction $ Mul    rX dmemaddr)
div   rX dmemaddr = tell (Instruction $ Div    rX dmemaddr)
ld    rX dmemaddr = tell (Instruction $ Load   rX dmemaddr)
st    rX dmemaddr = tell (Instruction $ Store  rX dmemaddr)
ldmi  rX dmemaddr = tell (Instruction $ LoadMI rX dmemaddr)
-- stmi  rX dmemaddr = tell (Instruction StoreMI $ rX dmemaddr)
-- cmpeq rX dmemaddr = tell (Instruction $ rX dmemaddr)
-- cmplt rX dmemaddr = tell (Instruction $ rX dmemaddr)
-- cmpgt rX dmemaddr = tell (Instruction $ rX dmemaddr)
-- sl    rX dmemaddr = tell (Instruction $ rX dmemaddr)
-- sr    rX dmemaddr = tell (Instruction $ rX dmemaddr)
-- sra   rX dmemaddr = tell (Instruction $ rX dmemaddr)

-- add_si rX simm = write 0b100000 (register rX .|. simm8 simm)
-- sub_si rX simm = write 0b100001 (register rX .|. simm8 simm)
-- mul_si rX simm = write 0b100010 (register rX .|. simm8 simm)
-- div_si rX simm = write 0b100011 (register rX .|. simm8 simm)
ld_si  rX simm = tell (Instruction $ Set rX simm)

-- sl_i   rX uimm = write 0b101100 (register rX .|. uimm8 uimm)
-- sr_i   rX uimm = write 0b101101 (register rX .|. uimm8 uimm)
-- sra_i  rX uimm = write 0b101110 (register rX .|. uimm8 uimm)
-- ld_i   rX uimm = write 0b101111 (register rX .|. uimm8 uimm)

jmpi    simm = tell (Instruction $ Jump     simm)
jmpiZ   simm = tell (Instruction $ JumpZero simm)
-- jmpi_ct simm = write 0b110001 (simm10 simm)
-- jmpi_cf simm = write 0b110010 (simm10 simm)
-- wait    uimm = write 0b110011 (uimm10 uimm)

-- not rX = write 0b111000 (register rX)
abs rX = tell (Instruction $ Abs rX)
halt   = tell (Instruction $ Halt)