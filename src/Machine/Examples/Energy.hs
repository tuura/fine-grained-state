module Machine.Examples.Energy where

import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import           Machine.Encode
import Machine.Examples.Common

energyEstimateProgram :: Program
energyEstimateProgram =
    let { t1 = 0; t2 = 1; p1 = 2; p2 = 3 }
    in zip [0..] $ map encode
    [ Instruction (Load  R0 t1)
    , Instruction (Sub   R0 t2)
    , Instruction (Abs   R0)
    , Instruction (Load  R1 p1)
    , Instruction (Add   R1 p2)
    , Instruction (Store R1 p2)
    , Instruction (Mul   R0 p2)
    , Instruction (Div   R0 t2)
    , Instruction Halt
    ]

energyEstimateExample :: IO ()
energyEstimateExample = do
    let steps = 30
        -- x = SConst 2
        -- y = SConst 3
        t1 = SAny 0
        t2 = SAny 1
        p1 = SAny 2
        p2 = SAny 3
        mem =  initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (5, SConst 100)]
        initialState = boot energyEstimateProgram mem
        trace =
                constraint overflowSet $
                constraint (const (p1 `SGt` (SConst 20))) $
                constraint (const (p1 `SLt` (SConst 30))) $
                constraint (const (p2 `SGt` (SConst 20))) $
                constraint (const (p2 `SLt` (SConst 30))) $
                constraint (const (t1 `SGt` (SConst 20))) $
                constraint (const (t1 `SLt` (SConst 30))) $
                constraint (const (t2 `SGt` (SConst 20))) $
                constraint (const (t2 `SLt` (SConst 30))) $
                runModel steps initialState
    solved <- SMT.solveTrace trace
    putStrLn $ renderSolvedTrace $ solved