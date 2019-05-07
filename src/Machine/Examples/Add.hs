module Machine.Examples.Add where

import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import           Machine.Encode
import Machine.Examples.Common

addProgram :: Program
addProgram = zip [0..] $ map encode
    [ Instruction (Load R0 0)
    , Instruction (Add R0 1)
    , Instruction (Halt)
    ]

addExample :: IO ()
addExample = do
    let steps = 15
        -- x = SConst 2
        -- y = SConst 3
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot addProgram mem
        trace =
                constraint "no overflow" overflowSet $
                constraint "x is in range" (const (x `SGt` (SConst 20))) $
                constraint "x is in range" (const (x `SLt` (SConst 30))) $
                constraint "y is in range" (const (y `SGt` (SConst 0)))  $
                constraint "y is in range" (const (y `SLt` (SConst 10))) $
                runModel steps initialState
    solved <- SMT.solveTrace trace
    putStrLn $ renderSolvedTrace $ solved