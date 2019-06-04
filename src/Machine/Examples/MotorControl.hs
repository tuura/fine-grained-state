module Machine.Examples.MotorControl where

import           Text.Pretty.Simple (pPrint)
import           Prelude hiding (div, mod)
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

motorControlExample :: Int -> IO ()
motorControlExample steps = do
    let -- steps = 100 -- steps must be enought for termination with dist = 101
        -- a_max = SConst 2
        -- v_max = SConst 30
        a_max = SAny 0
        v_max = SAny 1
        limit = 1000
        mem = initialiseMemory [(0, a_max), (1, v_max), (2, SConst 101)]
        initialState =
            appendConstraints [ ( "a_max is in range"
                                , (a_max `SGt` (SConst 0)) `SAnd` (a_max `SLt` (SConst limit))
                                )
                              , ( "v_max is in range"
                                , (v_max `SGt` (SConst 0)) `SAnd` (v_max `SLt` (SConst limit))
                                )
                              ] $
            boot (assemble motorControl) mem
    putStrLn $ "Initial constraints: " <> show (pathConstraintList initialState)
    trace <-
            -- constraint "no overflow" overflowSet $
            -- constraint  .
            -- constraint "v_max is in range" (const ((v_max `SGt` (SConst 0)) `SAnd` (v_max `SLt` (SConst 100)))) <$>
            -- constraint halted $
            runModel steps initialState
    putStrLn $ "Trace depth: " ++ show (traceDepth trace)
    let ps = paths (unTrace trace)
    putStrLn $ "Path in trace: " ++ show (length ps)



    -- -- Simulate with constants and observe the results
    -- let finalState = endOfPath (head ps)
    -- -- let s_final state = (Map.!) (memory state) 3
    -- --     v_final state = (Map.!) (memory state) 4
    -- --     halted  state = (Map.!) (flags state) Halted
    -- -- print (tryFoldConstant $ v_final finalState)
    -- -- print (halted finalState)
    -- mapM_ putStrLn (renderFinalValues finalState)

finalStates :: Trace s -> [s]
finalStates = map endOfPath . paths . unTrace

halted :: State -> Sym Bool
halted state = (Map.!) (flags state) Halted

extracetV :: State -> Sym Value
extracetV state = (Map.!) (memory state) 4

renderFinalValues :: State -> [String]
renderFinalValues state =
    let s_final = (Map.!) (memory state) 3
        v_final = (Map.!) (memory state) 4
        halted  = (Map.!) (flags state) Halted
    in [ "s_final == " <> show s_final
       , "v_final == " <> show v_final
       , "halted  == " <> show halted
       ]