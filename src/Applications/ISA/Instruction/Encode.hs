{-# LANGUAGE LambdaCase, TypeFamilies, FlexibleContexts #-}
module Applications.ISA.Instruction.Encode where

-- Decode the instruction AST from an enstruction code

import Data.Bits
import Applications.ISA.Types
import Applications.ISA.Instruction
import Data.Word (Word16)
import Data.Maybe (fromJust)

encode :: Instruction -> InstructionCode
encode = \case
    IF Halt -> 0
    IF (Load     r addr) -> fromBitsLE $ [f, f, f, f, f, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IS (LoadMI   r addr) -> fromBitsLE $ [f, f, f, f, t, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IF (Set      r byte) -> fromBitsLE $ [f, f, f, f, t, t] ++ encodeRegister r
                                                       ++ encodeByte byte
                                                       ++ pad 48
    IF (Store    r addr) -> fromBitsLE $ [f, f, f, t, f, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IA (Add      r addr) -> fromBitsLE $ [f, f, f, t, f, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IF (Jump     byte)   -> fromBitsLE $ [f, f, f, t, t, f] ++ encodeByte byte
                                                       ++ pad 50
    IS (JumpZero byte)   -> fromBitsLE $ [f, f, f, t, t, t] ++ encodeByte byte
                                                       ++ pad 50
    IA (Sub      r addr) -> fromBitsLE $ [f, f, t, f, f, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IA (Mul      r addr) -> fromBitsLE $ [f, f, t, f, f, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IA (Div      r addr) -> fromBitsLE $ [f, f, t, f, t, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IA (Mod      r addr) -> fromBitsLE $ [f, f, t, f, t, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    IA (Abs      r     ) -> fromBitsLE $ [f, f, t, t, f, f] ++ encodeRegister r
                                                       ++ pad 56
    where f = False
          t = True
          pad k = replicate k False

-- | 'Register' is encoded as a 2-bit word
encodeRegister :: Register -> [Bool]
encodeRegister = \case
    R0 -> [False, False]
    R1 -> [False, True]
    R2 -> [True, False]
    R3 -> [True, True]

-- | 'MemoryAddress' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeMemoryAddress :: MemoryAddress -> [Bool]
encodeMemoryAddress = take 8 . blastLE

-- | 'Byte' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeByte :: SImm8 -> [Bool]
encodeByte = take 8 . blastLE
--------------------------------------------------------------------------------
