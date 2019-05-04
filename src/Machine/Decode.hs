module Machine.Decode where

import Data.Bits
import Machine.Types

decode :: InstructionCode -> Instruction
decode code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [f, f, f, f, f, f] -> Instruction Halt
          | opcode == [f, f, f, f, f, t]  -> Instruction $
                Load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, f]   -> Instruction $
                LoadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, t]  -> Instruction $
                Set (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractSImm8 expandedCode)
          | opcode == [f, f, f, t, f, f]   -> Instruction $
                Store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, f, t]   -> Instruction $
                Add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, t, f]   -> Instruction $
                Jump (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, f, f, t, t, t]    -> Instruction $
                JumpZero (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, f, t, f, f, f]   -> Instruction $
                Sub (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, f, t]   -> Instruction $
                Mul (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, f]   -> Instruction $
                Div (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, t]   -> Instruction $
                Mod (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, t, f, f]   -> Instruction $
                Abs (decodeRegister . extractRegister $ expandedCode)
      where f = False
            t = True

fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []     = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

decodeRegister :: [Bool] -> Register
decodeRegister = \case
      [False, False] -> R0
      [False, True]  -> R1
      [True, False]  -> R2
      [True, True]   -> R3
      _              -> error $ "Machine.Instruction.Decode.decodeRegister:"
                             <> "register must be encoded as a two-bit word"

decodeOpcode :: [Bool] -> [Bool]
decodeOpcode = take 6

extractRegister :: [Bool] -> [Bool]
extractRegister = take 2 . drop 6

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = (++ pad 56) . take 8 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = (++ pad 56) . take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = (++ pad 56) . take 8 . drop 6

pad :: Int -> [Bool]
pad k = replicate k False
