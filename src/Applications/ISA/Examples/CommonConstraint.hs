{-# LANGUAGE RankNTypes #-}

module Applications.ISA.Examples.CommonConstraint where

import Control.Selective
-- import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import Applications.ISA.Types
import Applications.ISA.InstructionConstraint
-- import Applications.ISA.Instruction.Encode
-- import Applications.ISA.Instruction.Decode
-- import Applications.ISA.Program
import Applications.ISAConstraint
import Applications.ISA.SimulatorConstraint
import FS

type Program = [(InstructionAddress, Instruction1)]

--------------------------------------------------------------------------------
addExample :: Int -> MachineValue -> MachineValue -> IO ()
addExample steps x y = do
    -- prog <- readProgram "examples/add.asm"
    let prog = [  (0, Instr $ Load R0 0)
               ,  (1, Instr $ Add R0 1)
               ,  (2, Instr $ Halt)
               ]
    -- let prog = [  (0, encode . IF $ Halt)
    --            ]
    let mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        finalState = runModel steps initialState
    print . registers $ finalState
    print . instructionRegister $ finalState
    print . instructionCounter $ finalState
    print . flags $ finalState
    -- pPrint $ finalState
--------------------------------------------------------------------------------
-- gcdExample :: Int -> MachineValue -> MachineValue -> IO ()
-- gcdExample steps x y = do
--     prog <- readProgram "examples/gcd.asm"
--     let mem = initialiseMemory [(0, x), (1, y)]
--         initialState = boot prog mem
--         finalState = runModel steps initialState
--     print . registers $ finalState
--     print . registers $ finalState
--     print . instructionRegister $ finalState
--     print . instructionCounter $ finalState
--     print . clock $ finalState
--     print . flags $ finalState