module Machine.Examples.GCD where

import           System.IO.Unsafe (unsafePerformIO)
import           Control.Selective
import           Data.Foldable (sequenceA_)
-- import qualified Data.Tree as Tree
import qualified Data.SBV.Dynamic as SBV
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Semantics
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import qualified Data.Map.Strict  as Map
import           Machine.Encode
import           Machine.Examples.Common

gcdProgram :: Program
gcdProgram = zip [0..] $ map encode
    -- # Find the greatest common divisor of values in memory locations 0 and 1,
    -- # put result to the register R1
    [ Instruction (Set R0 0)
    , Instruction (Store R0 255)
    , Instruction (Load R0 1)
    -- # Test register R0 for being zero by subtracting zero
    , Instruction (Sub R0 255)
    -- # Halt if register R0 contains zero, loop otherwise
    , Instruction (JumpZero 6)
    , Instruction (Load R0 0)
    , Instruction (Mod R0 1)
    , Instruction (Load R1 1)
    , Instruction (Store R0 1)
    , Instruction (Store R1 0)
    , Instruction (Jump (-8))
    , Instruction Halt
    ]

gcdExample :: IO ()
gcdExample = do
    let steps = 15
        -- x = SConst 2
        -- y = SConst 3
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot gcdProgram mem
        trace =
                constraint overflowSet $
                constraint (const (x `SGt` (SConst 20))) $
                constraint (const (x `SLt` (SConst 30))) $
                constraint (const (y `SGt` (SConst 0)))  $
                constraint (const (y `SLt` (SConst 10))) $
                -- constraint halted $
                runModel steps initialState
    -- print gcdProgram
    -- putStrLn $ Tree.drawTree $ fmap renderState $ trace
    -- putStrLn $ unlines $ fmap renderState $ subsetTrace halted trace
    s <- SMT.solveTrace trace
    putStrLn $ "Trace depth: " ++ show (traceDepth trace)
    putStrLn $ renderSolvedTrace $ s
    -- putStrLn . unlines . fmap SMT.renderSolvedState $ queryTrace s
    -- print $ map SMT.renderSMTResult . map (\(SMT.SolvedState x y) -> y) $ queryTrace s