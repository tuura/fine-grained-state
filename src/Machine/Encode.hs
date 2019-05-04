module Machine.Encode where

import Data.Bits
import Machine.Types

encode :: Instruction -> InstructionCode
encode = \case
    Instruction Halt -> 0
    Instruction (Load     r addr) ->
        fromBitsLE $ [f, f, f, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (LoadMI   r addr) ->
        fromBitsLE $ [f, f, f, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Set      r byte) ->
        fromBitsLE $ [f, f, f, f, t, t] ++ encodeRegister r
                                        ++ encodeByte byte
                                        ++ pad 48
    Instruction (Store    r addr) ->
        fromBitsLE $ [f, f, f, t, f, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Add      r addr) ->
        fromBitsLE $ [f, f, f, t, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Jump     byte)   ->
        fromBitsLE $ [f, f, f, t, t, f] ++ encodeByte byte
                                        ++ pad 50
    Instruction (JumpZero byte)   ->
        fromBitsLE $ [f, f, f, t, t, t] ++ encodeByte byte
                                        ++ pad 50
    Instruction (Sub      r addr) ->
        fromBitsLE $ [f, f, t, f, f, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Mul      r addr) ->
        fromBitsLE $ [f, f, t, f, f, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Div      r addr) ->
        fromBitsLE $ [f, f, t, f, t, f] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Mod      r addr) ->
        fromBitsLE $ [f, f, t, f, t, t] ++ encodeRegister r
                                        ++ encodeMemoryAddress addr
                                        ++ pad 48
    Instruction (Abs      r)      ->
        fromBitsLE $ [f, f, t, t, f, f] ++ encodeRegister r
                                        ++ pad 56
    where f = False
          t = True
          pad k = replicate k f

fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []     = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

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