module Machine.Examples.Sum where

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


-- sumArrayLowLevel :: Script
-- sumArrayLowLevel = do
--     let { pointer = 0; sum = 253; const_two = 255 } -- ; pointer_store
--     ld_i r0 0
--     st r0 sum
--     ld r1 pointer
--     add_si r1 1
--     st r1 pointer

--     loop <- label
--     -- compare the pointer variable to the constant 2 (stored in the cell 255)
--     cmplt r1 const_two
--     -- if pointer == 2 then terminate
--     jmpi_ct 7

--     ldmi r2 pointer
--     add r2 sum
--     st r2 sum
--     ld r1 pointer
--     sub_si r1 1
--     st r1 pointer

--     goto loop
--     -- end loop
--     ld r0 sum
--     halt

sumArrayLowLevel :: Script
sumArrayLowLevel = do
    let { pointer = 0; sum = 253; const_one = 254; const_two = 255 }
    ld_si R0 0
    st R0 sum
    ld R1 pointer
    add R1 const_one
    st R1 pointer
    label "loop"
    sub R1 const_one
    gotoZ "end"
    ldmi R2 pointer
    add R2 sum
    st R2 sum
    ld R1 pointer
    sub R1 const_one
    st R1 pointer
    goto "loop"
    label "end"
    ld R0 sum
    halt

reg2HasResult :: State -> Sym Bool
reg2HasResult s =
    (Map.!) (registers s) R2 `SEq`
        (SAdd (SAny 1) (SAdd (SAny 2) (SAdd (SAny 3) (SConst 0))))

allSym :: [Sym Bool] -> Sym Bool
allSym = foldr SAnd (SConst True)

anySym :: [Sym Bool] -> Sym Bool
anySym = foldr SOr (SConst False)

collect :: (State -> Sym Bool) -> Path (Node State) -> Sym Bool
collect predicate path =
    -- foldr SAnd
    let conds = map (predicate . nodeBody) path
    in  anySym conds

sumExampleIO :: Int -> IO ()
sumExampleIO arraySize = do
    let steps = 10000 -- 40 is enough for (sum 3)
    -- 54 give an error: *** Exception: Map.!: given key is not an element in the map
    -- let names = map (("x" ++) . show) [1..arraySize]
    let summands = map SAny [1..arraySize]
    -- constrain xs to be in [0, 1000]
    -- let constr x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst $ 2 ^ 63 - 1)))
    let constr x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst 1000)))
    -- sequence_ (zipWith ($) (repeat constr) summands)
    let mem = initialiseMemory (zip [2..] summands ++
                                [(0, SConst . fromIntegral $ arraySize)] ++
                                [(254, SConst 1), (255, SConst 2)])
        initialState = boot (assemble sumArrayLowLevel) mem
        trace = constraint "no overflow" (SNot . overflowSet) $
                -- constraint "Halted" halted $
                constraint "Summands are in range" (const (allSym $ map constr summands)) $
                -- constraint "ResultIsCorrect" reg2HasResult $
                -- constraint (const (x `SGt` (SConst 0))) $
                -- constraint (const (x `SLt` (SConst 1000))) $
                -- constraint (const (y `SGt` (SConst 0)))  $
                -- constraint (const (y `SLt` (SConst 1000))) $
                runModel steps initialState
    -- putStrLn $ renderTrace (fmap foldConstantsInState trace)
    let ps = paths (unTrace trace)
        overflows =
            -- map (\b -> (SNot . halted . nodeBody $ last (head ps)) `SAnd` b) $
            map (last . take 1000 . iterate (tryFoldConstant . tryReduce)) $
            map ((allSym $ map constr summands) `SAnd`) $
            map (collect (\s ->
                    (Map.!) (flags s) Overflow)
                ) ps
        overflowVCs = map SMT.toSMT $ map (:[]) $ overflows
    satResults <- mapM (SBV.satWith SMT.prover) overflowVCs

    solved <- SMT.solveTrace (trace)
    print (length ps)
    -- let ls = map renderSolvedNode $ getSatStates solved
    -- mapM_ print ps
    -- putStrLn $ renderSolvedTrace $ solved

    mapM_ print (zip overflows satResults)
    mapM_ print (map ((registers . nodeBody . last)) ps)
    -- mapM_ print (satResults)
    -- mapM_ (\x -> print x >> putStrLn "---") $ zip satResults overflows

    -- mapM_ (\x -> print x >> putStrLn "---") ()
    -- putStrLn $ renderTrace (fmap foldConstantsInState trace)
    -- writeFile "trace.txt" $ (\(vs, es) -> vs <> "\n\n\n" <> es) $ Viz.renderDagrejs (Viz.mkGTrace trace)
    -- print $ Viz.renderDagrejs $ Viz.mkGTrace trace
    -- solved <- SMT.solveTrace (foldConstantsInTrace trace)
    -- solved <- SMT.solveTrace (trace)
    -- let ls = map renderSolvedNode $ getSatStates solved
    -- mapM_ putStrLn ls
    -- putStrLn $ renderSolvedTrace $ solved

sumExample :: Int -> IO (Sym Bool)
sumExample arraySize = do
    let steps = 100 -- 40 is enough for (sum 3)
    -- 54 give an error: *** Exception: Map.!: given key is not an element in the map
    -- let names = map (("x" ++) . show) [1..arraySize]
    let summands = map SAny [1..arraySize]
    -- constrain xs to be in [0, 1000]
    let constr x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst 1000)))
    -- sequence_ (zipWith ($) (repeat constr) summands)
    let mem = initialiseMemory (zip [2..] summands ++
                                [(0, SConst . fromIntegral $ arraySize)] ++
                                [(254, SConst 1), (255, SConst 2)])
        initialState = boot (assemble sumArrayLowLevel) mem
        trace = constraint "no overflow" (SNot . overflowSet) $
                constraint "Halted" halted $
                -- constraint "Summands are in range" (const (allSym $ map constr summands)) $
                -- constraint "ResultIsCorrect" reg2HasResult $
                -- constraint (const (x `SGt` (SConst 0))) $
                -- constraint (const (x `SLt` (SConst 1000))) $
                -- constraint (const (y `SGt` (SConst 0)))  $
                -- constraint (const (y `SLt` (SConst 1000))) $
                runModel steps initialState
    -- putStrLn $ renderTrace (fmap foldConstantsInState trace)
    let ps = paths (unTrace trace)
        overflows =
            map ((allSym $ map constr summands) `SAnd`) $
            map (last . take 1000 . iterate (tryFoldConstant . tryReduce)) $
            map (collect (\s ->
                    fromJust $ List.lookup "no overflow" $
                        pathConstraintList s)
                ) ps
        overflowVCs = map SMT.toSMT $ map (:[]) $ overflows
    satResults <- mapM (SBV.satWith SMT.prover) overflowVCs

    solved <- SMT.solveTrace (trace)
    print (length ps)
    -- let ls = map renderSolvedNode $ getSatStates solved
    -- mapM_ putStrLn ls
    -- putStrLn $ renderSolvedTrace $ solved

    -- mapM_ print (zip overflows satResults)
    -- mapM_ print (satResults)
    mapM_ (\x -> print x >> putStrLn "---") $ zip satResults overflows

    pure $ head overflows