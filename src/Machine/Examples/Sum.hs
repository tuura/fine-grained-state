module Machine.Examples.Sum where

import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import           Machine.Encode
import Machine.Examples.Common

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

sumArrayLowLevel :: Program
sumArrayLowLevel =
    let { pointer = 0; sum = 253; const_one = 254; const_two = 255 } in -- ; pointer_store
    zip [0..] $ map encode
    [ Instruction (Set R0 0)
    , Instruction (Store R0 sum)
    , Instruction (Load R1 pointer)
    , Instruction (Add R1 const_one)
    , Instruction (Store R1 pointer)
    -- , Instruction (loop <- label)
    , Instruction (Sub R1 const_two)
    , Instruction (JumpZero 8)
    , Instruction (LoadMI R2 pointer)
    , Instruction (Add R2 sum)
    , Instruction (Store R2 sum)
    , Instruction (Load R1 pointer)
    , Instruction (Sub R1 const_one)
    , Instruction (Store R1 pointer)
    , Instruction (Jump (-9))
    , Instruction (Load R0 sum)
    , Instruction (Halt)
    ]

sumExample :: Int -> IO ()
sumExample arraySize = do
    let steps = 40
        -- x = SConst 2
        -- y = SConst 3
        -- x = SAny 0
        -- y = SAny 1
    let names = map (("x" ++) . show) [1..arraySize]
    let summands = map SAny [1..length names]
    -- constrain xs to be in [0, 1000]
    -- sequence_ (zipWith ($) (repeat constr) summands)
    let mem = initialiseMemory (zip [2..] summands ++
                                [(0, SConst . fromIntegral $ arraySize)] ++
                                [(254, SConst 1), (255, SConst 2)])
        initialState = boot sumArrayLowLevel mem
        trace =
                -- constraint overflowSet $
                -- constraint (const (x `SGt` (SConst 20))) $
                -- constraint (const (x `SLt` (SConst 30))) $
                -- constraint (const (y `SGt` (SConst 0)))  $
                -- constraint (const (y `SLt` (SConst 10))) $
                runModel steps initialState
    -- putStrLn $ renderTrace trace
    -- putStrLn $ renderTrace (fmap foldConstantsInState trace)
    solved <- SMT.solveTrace (foldConstantsInTrace trace)
    -- solved <- SMT.solveTrace (trace)
    putStrLn $ renderSolvedTrace $ solved