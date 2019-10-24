module Machine.Examples.GCD where

import           System.CPUTime
import           Text.Printf
import           Prelude hiding (mod)
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Selective
import           Data.Foldable (sequenceA_)
-- import qualified Data.Tree as Tree
import qualified Data.SBV         as SBV
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

-- | Constrain variable's value to be in [0, 1000]
inRange :: Sym Value -> Sym Bool
inRange x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst 1000)))

gcdExample :: Int -> IO ()
gcdExample steps = do
    putStrLn "--------------------------------------------------"
    let constr x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst 1000)))
    let -- x = SConst 2
        -- y = SConst 3
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot (assemble gcdProgram) mem
        trace = runModel steps initialState
    let ps = paths (unTrace trace)
    putStrLn $ "Non-trivial paths: " <> show (length ps)
    putStrLn "--------------------------------------------------"
    mapM_ (processPath (allSym $ map inRange [x, y])) (zip [1..] ps)
    where
        processPath :: Sym Bool -> (Int, Path (Node State)) -> IO ()
        processPath preconditions (pathId, path) = do
            putStrLn $ "Path id: " <> show pathId
            putStrLn $ "Nodes in path: " <> show (length path)
            let symVC =
                    preconditions           `SAnd`
                    (SNot $ pathHalts path)
                    `SAnd`
                    findOverflowInPath path
                sbvVC = SMT.toSMT [symVC]
            putStrLn $ "Path id: "       <> show      pathId
            putStrLn $ "Nodes in path: " <> show (length path)
            dead <- isDead preconditions path
            if dead then putStrLn "dead"
            else do
                let sbvVC = SMT.toSMT [symVC]
                satStart  <- getCPUTime
                satResult <- SBV.satWith SMT.prover sbvVC
                satFinish <- getCPUTime
                -- putStrLn $ "Find VC : "      <> show symVC
                putStrLn $ show satResult
            putStrLn $ "--------------------------------------------"
            -- satStart <- getCPUTime
            -- satResult <- SBV.satWith SMT.prover overflowSbvVC
            -- satFinish <- getCPUTime
            -- putStrLn $ "Find overflow VC : " <> show satResult
            -- let diff = (fromIntegral (satFinish - satStart)) / (10^12)
            -- printf "SAT Computation time: %0.3f sec\n" (diff :: Double)
            -- -- putStrLn $ "GCD symbolic expression: " <> show (result path)
            -- putStrLn "--------------------------------------------------"
