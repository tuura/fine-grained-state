module Machine.Examples.Energy where

import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import           Machine.Encode
import Machine.Examples.Common

import           Data.Metrology.Poly
import           Data.Metrology.SI ()  -- DefaultLCSU instances
import           qualified Data.Metrology.SI.PolyTypes as SI
import           Data.Units.SI
import           Data.Units.SI.Prefixes
import           Data.Metrology.Show
import           Data.Metrology.Unsafe

data Year = Year
instance Unit Year where
    type BaseUnit Year = Second
    conversionRatio _ = 60 * 60 * 24 * 366

type Time = SI.Time 'DefaultLCSU Value

type Power = SI.Power 'DefaultLCSU Value

type Energy = Time %* Power

-- toMilliSeconds :: Time -> Value
-- toMilliSeconds t = (t # milli Second)

-- toMilliWatts :: Power -> Value
-- toMilliWatts p = (p # milli Watt)

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

    -- constrain $ t1 .>= 0 &&& t1 .<= toMilliSeconds (30 % Year)
    -- constrain $ t2 .>= 0 &&& t2 .<= toMilliSeconds (30 % Year)
    -- constrain $ p1 .>= 0 &&& p1 .<= toMilliWatts (1 % Watt)
    -- constrain $ p2 .>= 0 &&& p2 .<= toMilliWatts (1 % Watt)

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
                -- constraint overflowSet $
                -- constraint (const (t1 `SGt` (SConst 0))) $
                -- constraint (const (t1 `SLt` (SConst $ toMilliSeconds (30 % Year)))) $
                -- constraint (const (t2 `SGt` (SConst 0))) $
                -- constraint (const (t2 `SLt` (SConst $ toMilliSeconds (30 % Year)))) $
                -- constraint (const (p1 `SGt` (SConst 0))) $
                -- constraint (const (p1 `SLt` (SConst $ toMilliWatts (1 % Watt)))) $
                -- constraint (const (p2 `SGt` (SConst 0)))
                -- constraint (const (p2 `SLt` (SConst $ toMilliWatts (1 % Watt)))) $
                constraint "no overflow" overflowSet $
                constraint "t1 is in range" (const (t1 `SGt` (SConst 0))) $
                constraint "t1 is in range" (const (t1 `SLt` (SConst 948672000000))) $
                constraint "t2 is in range" (const (t2 `SGt` (SConst 0))) $
                constraint "t2 is in range" (const (t2 `SLt` (SConst 948672000000))) $
                constraint "p1 is in range" (const (p1 `SGt` (SConst 0))) $
                constraint "p1 is in range" (const (p1 `SLt` (SConst 1000))) $
                constraint "p2 is in range" (const (p2 `SGt` (SConst 0))) $
                constraint "p2 is in range" (const (p2 `SLt` (SConst 1000))) $
                runModel steps initialState
    solved <- SMT.solveTrace trace
    putStrLn $ renderSolvedTrace $ solved