module Machine.Examples.MotorControl where

import           System.CPUTime
import           Text.Printf
import           Text.Pretty.Simple (pPrint)
import           Prelude hiding (div, mod)
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Selective
import           Data.Maybe (fromJust)
import           Data.Foldable (sequenceA_)
-- import qualified Data.Tree as Tree
import qualified Data.SBV as SBV
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Decode
import           Machine.Symbolic
import           Machine.Assembly
import qualified Machine.Semantics as S
import qualified Machine.SMT      as SMT
import qualified Data.Map.Strict  as Map
import           Machine.Encode
import           Machine.Examples.Common


-- | Example program
motorControl :: Script
motorControl = do -- Don't forget the 'do'!
    -- Declare named memory locations if necessary
    let { a_max = 0; v_max = 1; dist = 2; s = 3; v = 4; s_decel = 5;
          decel_steps = 6; temp = 7; v_next = 8; }
    -- the four registers must be referred as r0, r1, r2, r3, like in the
    -- following stub command:

    -- compute s_decel
    "start" @@ ld r0 v
    div r0 a_max
    st r0 decel_steps
    ld_si r1 1
    st r1 temp
    add r0 temp
    st r0 temp
    ld r0 a_max
    mul r0 decel_steps
    mul r0 temp
    -- sra_i r0 1
    div r0 a_max

    ld r1 decel_steps
    mul r1 a_max
    cmpeq r1 v
    goto_ct "store_s_decel"
    add r0 v
    "store_s_decel" @@ st r0 s_decel

    -- compute v_next
    ld r0 v
    add r0 a_max                        -- v_next = v + a_max
    cmplt r0 dist                       -- v_next < dist ?
    goto_ct "keep_v_next1"
    ld r0 dist                          -- overwrite v_next with dist
    "keep_v_next1" @@ cmplt r0 v_max    -- v_next < v_max ?
    goto_ct "keep_v_next2"
    ld r0 v_max                         -- overwrite v_next with v_max
    "keep_v_next2" @@ st r0 v_next      -- store v_next value

    -- set speed according to final distance
    add r0 s_decel
    add r0 s
    cmpgt r0 dist                       -- s + s_decel + v_next > dist ?
    goto_ct "keep_speed"
    ld r1 v_next                        -- accelerate
    goto "set_v"
    "keep_speed" @@ ld r0 s
    add r0 s_decel
    add r0 v
    cmpgt r0 dist                       -- s + s_decel + v > dist ?
    goto_ct "decelerate"
    ld r1 v
    goto "set_v"
    "decelerate" @@ ld r0 v
    ld r1 decel_steps
    mul r1 a_max                       -- n * a_max
    st r1 temp
    cmpgt r0 temp                      -- v > n * a_max ?
    goto_ct "set_v"
    ld r1 v
    sub r1 a_max
    "set_v" @@ st r1 v

    -- speed check
    ld_si r0 0
    st r0 temp
    cmpeq r1 temp                     -- v == 0?
    goto_cf "inc_s"                   -- speed is non-zero: continue

    -- in case speed is 0, check distance covered:
    ld r0 s
    cmpeq r0 dist
    goto_cf "reaccelerate"
    halt                              -- we have reached our destination
    "reaccelerate" @@ ld r0 dist
    sub r0 s                          -- dist - s
    cmplt r0 a_max
    goto_ct "set_v2"
    ld r0 a_max
    "set_v2" @@ st r0 v

    "inc_s" @@ ld r0 s
    add r0 v
    st r0 s
    goto "start"

-- motorControlExample :: Int -> IO ()
-- motorControlExample steps = do
--     let -- steps = 100 -- steps must be enought for termination with dist = 101
--         -- a_max = SConst 2
--         -- v_max = SConst 30
--         a_max = SAny 0
--         v_max = SAny 1
--         dist  = SAny 2
--         mem = initialiseMemory [(0, a_max), (1, v_max), (2, dist)]
--         initialState = boot (assemble motorControl) mem
--         trace =
--                 -- constraint "no overflow" overflowSet $
--                 -- constraint "x is in range" (const (x `SGt` (SConst 20))) $
--                 -- constraint "x is in range" (const (x `SLt` (SConst 30))) $
--                 -- constraint "y is in range" (const (y `SGt` (SConst 0)))  $
--                 -- constraint "y is in range" (const (y `SLt` (SConst 10))) $
--                 -- constraint halted $
--                 runModel steps initialState
--     let ps = paths (unTrace trace)
--     putStrLn $ "Non-trivial paths: " <> show (length ps)
--     putStrLn "--------------------------------------------------"
--     -- putStrLn $ renderTrace trace
--     mapM_ (processPath' (inRangeA a_max `SAnd` inRangeV v_max)) (zip [1..] ps)
--     where
--         processPath :: Sym Bool -> (Int, Path (Node State)) -> IO ()
--         processPath preconditions (pathId, path) = do
--             putStrLn $ "Path id: "       <> show      pathId
--             putStrLn $ "Nodes in path: " <> show (length path)
--             let overflowSymVC =
--                     preconditions           `SAnd`
--                     (SNot $ pathHalts path)
--                     -- `SAnd`
--                     -- -- findOverflowInPath path
--                     `SAnd`
--                     (SNot $ v_final_is_0 path)
--                 overflowSbvVC = SMT.toSMT [overflowSymVC]
--             satStart <- getCPUTime
--             satResult <- SBV.satWith SMT.prover overflowSbvVC
--             satFinish <- getCPUTime
--             putStrLn $ "Find overflow VC : " <> show satResult
--             let diff = (fromIntegral (satFinish - satStart)) / (10^12)
--             printf "SAT Computation time: %0.3f sec\n" (diff :: Double)
--             putStrLn $ "VC: " <> show (overflowSymVC)
--             putStrLn "--------------------------------------------------"

--         processPath' :: Sym Bool -> (Int, Path (Node State)) -> IO ()
--         processPath' preconditions (pathId, path) = do
--             putStrLn $ "Path id: "       <> show      pathId
--             putStrLn $ "Nodes in path: " <> show (length path)
--             putStrLn $ "Jumps in path: " <> show (countJumps path)
--             putStrLn "--------------------------------------------------"

v_final_is_0 :: Path (Node State) -> Sym Bool
v_final_is_0 path =
    SEq (SConst 0)
        ((\s -> (Map.!) (registers s) R1) . nodeBody . last $ path)

    -- -- Simulate with constants and observe the results
    -- let finalState = endOfPath (head ps)
    -- -- let s_final state = (Map.!) (memory state) 3
    -- --     v_final state = (Map.!) (memory state) 4
    -- --     halted  state = (Map.!) (flags state) Halted
    -- -- print (tryFoldConstant $ v_final finalState)
    -- -- print (halted finalState)
    -- mapM_ putStrLn (renderFinalValues finalState)

extractV :: State -> Sym Value
extractV state = (Map.!) (memory state) 4

renderFinalValues :: State -> [String]
renderFinalValues state =
    let s_final = (Map.!) (memory state) 3
        v_final = (Map.!) (memory state) 4
        halted  = (Map.!) (flags state) Halted
    in [ "s_final == " <> show s_final
       , "v_final == " <> show v_final
       , "halted  == " <> show halted
       ]

------------------------------------------------------------
-- motorControlSemantics :: FS Selective ()
-- motorControlSemantics = foldl1 (flip (*>))   $
--     map instructionSemantics (map (decode . snd) $ assemble motorControl)

---------------- Loop Body Analysis --------------------------------------------
motorControlBodyExample :: Int -> IO ()
motorControlBodyExample steps = do
    let -- steps = 100 -- steps must be enought for termination with dist = 101
        -- a_max = SConst 2
        -- v_max = SConst 30
        a_max = SAny 0
        v_max = SAny 1
        dist  = SAny 2
        s     = SAny 3
        v     = SAny 4
        mem = initialiseMemory [ (0, a_max)
                               , (1, v_max)
                               , (2, dist)
                               , (3, s)
                               , (4, v)
                               ]
        initialState = boot (assemble mc_loop) mem
        trace =
                -- constraint "no overflow" overflowSet $
                -- constraint "x is in range" (const (x `SGt` (SConst 20))) $
                -- constraint "x is in range" (const (x `SLt` (SConst 30))) $
                -- constraint "y is in range" (const (y `SGt` (SConst 0)))  $
                -- constraint "y is in range" (const (y `SLt` (SConst 10))) $
                -- constraint halted $
                runModel steps initialState
    let ps = paths (unTrace trace)
    putStrLn $ "Non-trivial paths: " <> show (length ps)
    putStrLn "--------------------------------------------------"
    -- putStrLn $ renderTrace trace
    let preconditions =
            ((v `SGt` (SConst $ -1)) `SAnd` (v `SLt` v_max)) `SAnd`
            ((s `SGt` (SConst $ -1)) `SAnd` (s `SLt` dist )) `SAnd`
            (SLt a_max v_max)        `SAnd`
            inRange_a_max a_max      `SAnd`
            inRange_v_max v_max
    mapM_ (processPath preconditions) (zip [1..] ps)
    where
        inRange_a_max :: Sym Value -> Sym Bool
        inRange_a_max x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst $ 2^16)))

        inRange_v_max :: Sym Value -> Sym Bool
        inRange_v_max x = (x `SGt` (SConst 0) `SAnd` (x `SLt` (SConst $ 2^16)))

        v_safety :: Path (Node State) -> Sym Bool
        v_safety path = SLt (extractV (nodeBody . last $ path))
                            (SAdd (SConst 1) (SAny 1))

        pathConstraint :: Path (Node State) -> Sym Bool
        pathConstraint path =
            allSym $ map snd . pathConstraintList . nodeBody . last $ path

        processPath :: Sym Bool -> (Int, Path (Node State)) -> IO ()
        processPath preconditions (pathId, path) = do
            let overflowSymVC =
                    preconditions           `SAnd`
                    -- (SNot $ pathHalts path) `SAnd`
                    (pathConstraint path)   `SAnd`
                    (SNot $ v_safety path)
                    -- `SAnd`
                    -- -- findOverflowInPath path
                    -- `SAnd`
                    -- (SNot $ v_final_is_0 path)
                overflowSbvVC = SMT.toSMT [overflowSymVC]
            satStart <- getCPUTime
            satResult <- SBV.satWith SMT.prover overflowSbvVC
            satFinish <- getCPUTime
            -- if (isSat satResult) then do
            putStrLn $ "Path id: "       <> show      pathId
            putStrLn $ "Nodes in path: " <> show (length path)
            putStrLn $ "Find VC : " <> show satResult
            -- else pure ()
            -- let diff = (fromIntegral (satFinish - s7---------------------"

        -- processPath' :: Sym Bool -> (Int, Path (Node State)) -> IO ()
        -- processPath' preconditions (pathId, path) = do
        --     putStrLn $ "Path id: "       <> show      pathId
        --     putStrLn $ "Nodes in path: " <> show (length path)
        --     putStrLn $ "Jumps in path: " <> show (countJumps path)
        --     putStrLn "--"

isSat :: SBV.SatResult -> Bool
isSat = \case
    (SBV.SatResult (SBV.Satisfiable _ _)) -> True
    _                                     -> False

mc_loop :: Script
mc_loop = do
    let { a_max = 0; v_max = 1; dist = 2; s = 3; v = 4; s_decel = 5;
          decel_steps = 6; temp = 7; v_next = 8; }
    ld r0 v
    div r0 a_max
    st r0 decel_steps
    ld_si r1 1
    st r1 temp
    add r0 temp
    st r0 temp
    ld r0 a_max
    mul r0 decel_steps
    mul r0 temp
    -- sra_i r0 1
    div r0 a_max

    ld r1 decel_steps
    mul r1 a_max
    cmpeq r1 v
    goto_ct "store_s_decel"
    add r0 v
    "store_s_decel" @@ st r0 s_decel

    -- compute v_next
    ld r0 v
    add r0 a_max                        -- v_next = v + a_max
    cmplt r0 dist                       -- v_next < dist ?
    goto_ct "keep_v_next1"
    ld r0 dist                          -- overwrite v_next with dist
    "keep_v_next1" @@ cmplt r0 v_max    -- v_next < v_max ?
    goto_ct "keep_v_next2"
    ld r0 v_max                         -- overwrite v_next with v_max
    "keep_v_next2" @@ st r0 v_next      -- store v_next value

    -- set speed according to final distance
    add r0 s_decel
    add r0 s
    cmpgt r0 dist                       -- s + s_decel + v_next > dist ?
    goto_ct "keep_speed"
    ld r1 v_next                        -- accelerate
    goto "set_v"
    "keep_speed" @@ ld r0 s
    add r0 s_decel
    add r0 v
    cmpgt r0 dist                       -- s + s_decel + v > dist ?
    goto_ct "decelerate"
    ld r1 v
    goto "set_v"
    "decelerate" @@ ld r0 v
    ld r1 decel_steps
    mul r1 a_max                       -- n * a_max
    st r1 temp
    cmpgt r0 temp                      -- v > n * a_max ?
    goto_ct "set_v"
    ld r1 v
    sub r1 a_max
    "set_v" @@ st r1 v

    -- speed check
    ld_si r0 0
    st r0 temp
    cmpeq r1 temp                     -- v == 0?
    goto_cf "inc_s"                   -- speed is non-zero: continue

    -- in case speed is 0, check distance covered:
    ld r0 s
    cmpeq r0 dist
    goto_cf "reaccelerate"
    halt                              -- we have reached our destination
    "reaccelerate" @@ ld r0 dist
    sub r0 s                          -- dist - s
    cmplt r0 a_max
    goto_ct "set_v2"
    ld r0 a_max
    "set_v2" @@ st r0 v

    "inc_s" @@ ld r0 s
    add r0 v
    st r0 s

    halt
