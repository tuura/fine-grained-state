{-# LANGUAGE RankNTypes #-}

module Applications.ISA.Examples.Common where

import Control.Selective
-- import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import Applications.ISA.Types
import Applications.ISA.Instruction
-- import Applications.ISA.Instruction.Encode
-- import Applications.ISA.Instruction.Decode
import Applications.ISA.Program
import Applications.ISA
import Applications.ISA.Simulator
import FS

--------------------------------------------------------------------------------
addExample :: Int -> MachineValue -> MachineValue -> IO ()
addExample steps x y = do
    -- prog <- readProgram "examples/add.asm"
    let prog = [  (0, IF $ Load R0 0)
               ,  (1, IA $ Add R0 1)
               ,  (2, IF $ Halt)
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
gcdExample :: Int -> MachineValue -> MachineValue -> IO ()
gcdExample steps x y = do
    prog <- readProgram "examples/gcd.asm"
    let mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        finalState = runModel steps initialState
    print . registers $ finalState
    print . registers $ finalState
    print . instructionRegister $ finalState
    print . instructionCounter $ finalState
    print . clock $ finalState
    print . flags $ finalState