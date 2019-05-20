module Machine.Examples.Sum where

import           System.CPUTime
import           Text.Printf
import qualified Data.Map         as Map
import           Control.Arrow (second)
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Assembly
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import           Machine.Encode
import           Machine.Decode
import qualified Machine.Types.Trace.Viz as Viz
import           Machine.Examples.Common
import qualified Algebra.Graph            as G
import           Text.Pretty.Simple (pPrint)
import qualified Data.SBV        as SBV
import qualified Data.List       as List
import           Data.Maybe (fromJust)

sumArrayLowLevel :: Script
sumArrayLowLevel = do
    let { pointer = 0; sum = 253; const_one = 254; const_two = 255 }
    ld_si r0 0
    st r0 sum
    ld r1 pointer
    add r1 const_one
    st r1 pointer

    -- compare the pointer variable to the constant 2 (stored in the cell 255)
    "loop" @@ cmplt r1 const_two
    -- if pointer < 2 then terminate
    goto_ct "end"
    -- otherwise: add next term to the sum

    ldmi r2 pointer
    add r2 sum
    st r2 sum
    ld r1 pointer
    sub r1 const_one
    st r1 pointer

    goto "loop"
    "end" @@ ld r0 sum
    halt

reg2HasResult :: State -> Sym Bool
reg2HasResult s =
    (Map.!) (registers s) R2 `SEq`
        (SAdd (SAny 1) (SAdd (SAny 2) (SAdd (SAny 3) (SConst 0))))

-- | Constrain variable's value to be in [0, 1000]
inRange :: Sym Value -> Sym Bool
inRange x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst 1000)))

--  x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst $ 2 ^ 63 - 1)))

sumExampleIO :: Int -> IO ()
sumExampleIO arraySize = do
    let steps = 10000 -- 40 is enough for (sum 3)
    let summands = map SAny [1..arraySize]
    let mem = initialiseMemory (zip [2..] summands ++
                                [ (0, SConst . fromIntegral $ arraySize)] ++
                                [ (254, SConst 1)
                                , (255, SConst 2)])
        initialState = boot (assemble sumArrayLowLevel) mem
        trace = runModel steps initialState
    let ps = paths (unTrace trace)
    putStrLn $ "Non-trivial paths: " <> show (length ps)
    mapM_ (processPath (allSym $ map inRange summands)) ps
    where
        processPath :: Sym Bool -> Path (Node State) -> IO ()
        processPath preconditions path = do
            putStrLn $ "Nodes in path: " <> show (length path)
            let overflowSymVC =
                    preconditions           `SAnd`
                    (SNot $ pathHalts path)
                    `SAnd`
                    findOverflowInPath path
                overflowSbvVC = SMT.toSMT [overflowSymVC]
            satStart <- getCPUTime
            satResult <- SBV.satWith SMT.prover overflowSbvVC
            satFinish <- getCPUTime
            putStrLn $ "Find overflow VC : " <> show satResult
            let diff = (fromIntegral (satFinish - satStart)) / (10^12)
            printf "SAT Computation time: %0.3f sec\n" (diff :: Double)