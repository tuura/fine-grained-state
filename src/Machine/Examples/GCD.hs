module Machine.Examples.GCD where

import           Prelude hiding (mod)
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Selective
import           Data.Foldable (sequenceA_)
-- import qualified Data.Tree as Tree
import qualified Data.SBV.Dynamic as SBV
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
-- import           Machine.Semantics
import           Machine.Symbolic
import           Machine.Assembly
import qualified Machine.SMT      as SMT
import qualified Data.Map.Strict  as Map
import           Machine.Encode
import           Machine.Examples.Common

gcdProgram :: Script
gcdProgram = do
    -- # Find the greatest common divisor of values in memory locations 0 and 1,
    -- # put result to the register R1
    ld_si r0 0
    st    r0 255
    ld    r0 1
    -- # Test register R0 for being zero
    "loop" @@ cmpeq r0 255
    goto_ct "end"
    ld r0 0
    mod r0 1
    ld r1 1
    st r0 1
    st r1 0
    goto "loop"
    "end" @@ halt

gcdExample :: IO ()
gcdExample = do
    let constr x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst 1000)))
    let steps = 10
        -- x = SConst 2
        -- y = SConst 3
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot (assemble gcdProgram) mem
        trace =
                constraint "no overflow" overflowSet $
                -- constraint "x is in range" (const (x `SGt` (SConst 20))) $
                -- constraint "x is in range" (const (x `SLt` (SConst 30))) $
                -- constraint "y is in range" (const (y `SGt` (SConst 0)))  $
                -- constraint "y is in range" (const (y `SLt` (SConst 10))) $
                -- constraint halted $
                runModel steps initialState
    -- print gcdProgram
    -- putStrLn $ Tree.drawTree $ fmap renderState $ trace
    -- putStrLn $ unlines $ fmap renderState $ subsetTrace halted trace

    -- s <- SMT.solveTrace trace
    putStrLn $ "Trace depth: " ++ show (traceDepth trace)
    let ps = paths (unTrace trace)
    putStrLn $ "Path in trace: " ++ show (length ps)
    let overflows =
            -- map (\b -> (SNot . halted . nodeBody $ last (head ps)) `SAnd` b) $
            map (last . take 1000 . iterate (tryFoldConstant . tryReduce)) $
            -- map ((allSym $ map constr summands) `SAnd`) $
            map (collect (\s ->
                    (Map.!) (flags s) Overflow)
                ) ps
        overflowVCs = map SMT.toSMT $ map (:[]) $ overflows
    satResults <- mapM (SBV.satWith SMT.prover) overflowVCs
    mapM_ print (zip overflows satResults)
    -- putStrLn $ renderTrace $ trace
    -- putStrLn $ renderSolvedTrace $ s
    -- putStrLn . unlines . fmap SMT.renderSolvedState $ queryTrace s
    -- print $ map SMT.renderSMTResult . map (\(SMT.SolvedState x y) -> y) $ queryTrace s