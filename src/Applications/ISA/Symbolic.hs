{-# LANGUAGE LambdaCase, GADTs, TypeFamilies
            , RankNTypes, MultiParamTypeClasses
            , DeriveFunctor, StandaloneDeriving, ScopedTypeVariables #-}

module Applications.ISA.Symbolic (
    -- * State of IAM machine
    -- MachineState (..),

    -- * Create initial state
    initialiseMemory, boot,

    -- * Dump a chunk of memory
    -- dumpMemory,

    -- * Simulate the machine execution
    -- runModel
    ) where

import Control.Monad.State
import Control.Selective
import Prelude hiding (Monad)
import qualified Prelude (Monad)
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import FS
import Applications.ISAParam
import Applications.ISA.Types
import Applications.ISA.Instruction
import Applications.ISA.Instruction.Decode
import Applications.ISA.Instruction.Encode
import Applications.ISA.Program hiding (readProgram)
-- import Applications.ISAWeaken hiding (jumpZero)
import Applications.ISA.Symbolic.Types
import Applications.ISA.Symbolic.SMT hiding (Sym(..))

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data SymEngine a = SymEngine
    { runSymEngine :: SymState -> [(a, SymState)] }
    deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative SymEngine where
    pure  = return
    (<*>) = ap

-- | Conditionally perform an effect.
whenSym :: SymEngine (Sym Bool) -> SymEngine () -> SymEngine ()
whenSym cond comp = -- select (bool (Right ()) (Left ()) <$> x) (const <$> y)
    SymEngine $ \s -> do
        a@(evalCond, s') <- runSymEngine cond s
        b@(compRes, s'') <- runSymEngine comp s'
        (f a (snd b))
        -- pure ((), appendConstraints [evalCond] s')
        -- pure ((), appendConstraints [(SNot evalCond)] s'')
    -- concat [f a (snd b) | a <- runSymEngine cond s, b <- runSymEngine comp (snd a)]
    where
        f :: (Sym Bool, SymState) -> SymState -> [((), SymState)]
        f (b, sNoExec) sOnExec =
            [ ((), appendConstraints [b] sOnExec)
            , ((), appendConstraints [(SNot b)] sNoExec)]

instance Selective SymEngine where
    select = selectM

instance Prelude.Monad SymEngine where
    return a       = SymEngine $ \s -> [(a, s)]
    SymEngine r >>= f = SymEngine $ \s ->
        let outcomes = r s
        in concat $ map (\(result, state) -> runSymEngine (f result) state) outcomes

instance (MonadState SymState) SymEngine where
    get = getState
    put = putState

getState :: SymEngine SymState
getState = SymEngine $ \s -> [(s, s)]

putState :: SymState -> SymEngine ()
putState s = SymEngine $ \_ -> [((), s)]

appendConstraints :: [Sym Bool] -> SymState -> SymState
appendConstraints cs s =
    let cs' = cs ++ pathConstraintList s
    in s { pathConstraintList = cs' }

-- | Instance of the Machine.Metalanguage read command for symbolic execution
readKey :: MachineKey Sym (Sym a) -> SymEngine (Sym a)
readKey = \case
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> SConst . instructionCounter <$> get
    -- IR        -> SConst <$> readInstructionRegister
    -- Prog addr -> SConst <$> readProgram addr

-- | Instance of the Machine.Metalanguage write command for symbolic execution
writeKey :: MachineKey Sym (Sym a)
         -> SymEngine (Sym a)
         -> SymEngine ()
writeKey k v = case k of
    Reg  reg  -> v >>= writeRegister reg
    Addr addr -> v >>= writeMemory   addr
    F    flag -> v >>= writeFlag flag
    IC        -> v >>= \case
        (SConst val) -> modify $ \currentState -> currentState {instructionCounter = val}
        _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IC is not supported"
    IR        -> v >>= \case
        (SConst val) -> writeInstructionRegister val
        _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IR is not supported"
    Prog _    -> error "Machine.Semantics.Symbolic: Can't write Program"

symStep :: SymState -> [SymState]
symStep state =
    let [(instrCode, fetched)] = (flip runSymEngine) state $ do
                                    fetchInstruction
                                    incrementInstructionCounter
                                    readInstructionRegister
        i = decode instrCode
    in -- (snd . ((flip runSymEngine) fetched)) <$>
       case i of
        --   (Instruction (JumpZero offset)) -> map snd $ runSymEngine (jumpZero offset) fetched
          _ -> map snd $
            runSymEngine (instructionSemantics' i readKey writeKey) fetched

-- -- runModel :: Int -> SymState -> Trace
-- -- runModel steps state
-- --     | steps <= 0 = Tree.Node state []
-- --     | otherwise  = if halted then Tree.Node state [] else Tree.Node state children
-- --   where
-- --     halted    = (Map.!) (flags state) Halted /= (SConst 0)
-- --     newStates = symStep state
-- --     children  = runModel (steps - 1) <$> newStates
-- -- --------------------------------------------------------------------------------
-- -- ----------- The IAM microcommands in terms of SymEngine
-- -- --------------------------------------------------------------------------------

--- -- ---------- Clock -------------------------------------------------------------
-- -- --------------------------------------------------------------------------------

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> SymEngine ()
delay cycles =
    modify $ \currentState ->
        currentState {clock = clock currentState + cycles}

-- --------------------------------------------------------------------------------
-- ------------ Memory ------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> Sym Value -> SymEngine ()
writeMemory address value = do
    delay 2
    modify $ \currentState ->
        currentState {memory =
            Map.adjust (const value) (fromIntegral address) (memory currentState)}

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0. We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: MemoryAddress -> SymEngine (Sym Value)
readMemory address = do
    currentState <- getState
    delay 2
    pure $ (Map.!) (memory currentState) (fromIntegral address)

-- -- --------------------------------------------------------------------------------
-- -- ------------ Registers ---------------------------------------------------------
-- -- --------------------------------------------------------------------------------

-- | Lookup the 'Val ue' in a given 'Register'. If the register has never been
-- initialised, this function returns 0. We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: Register -> SymEngine (Sym Value)
readRegister register = do
    s <- getState
    delay 1
    pure $ (Map.!) (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Sym Value -> SymEngine ()
writeRegister register value = do
    delay 1
    modify $ \currentState ->
        currentState {registers = Map.adjust (const value) register (registers currentState)}

-- -- --------------------------------------------------------------------------------
-- -- ------------ Flags ---------------------------------------------------------
-- -- --------------------------------------------------------------------------------

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> SymEngine (Sym Bool)
readFlag flag = do
    currentState <- getState
    pure $ (Map.!) (flags currentState) flag
    -- SymEngine $ \s ->
    --     let v = (Map.!) (flags s) flag
    --     in [ (v, appendConstraints [v] s)
    --        , (v, appendConstraints [(SNot v)] s)]

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Sym Bool -> SymEngine ()
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = Map.adjust (const value) flag (flags currentState)
        }

-- -- --------------------------------------------------------------------------------
-- -- ------------ Program -----------------------------------------------------------
-- -- --------------------------------------------------------------------------------

-- | Increment the instruction counter.
incrementInstructionCounter :: SymEngine ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = (+) 1 (instructionCounter currentState)}

fetchInstruction :: SymEngine ()
fetchInstruction = do
    ic <- instructionCounter <$> getState
    readProgram ic >>= writeInstructionRegister

readProgram :: InstructionAddress -> SymEngine (InstructionCode)
readProgram addr = do
    currentState <- getState
    delay 1
    pure . snd $ (!!) (program currentState) (fromIntegral addr)

readInstructionRegister :: SymEngine InstructionCode
readInstructionRegister = instructionRegister <$> getState

writeInstructionRegister :: InstructionCode -> SymEngine ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}

-- --------------------------------------------------------------------------------
-- ------------------ Examples ----------------------------------------------------
-- --------------------------------------------------------------------------------

-- -- gcdExample :: IO ()
-- -- gcdExample = do
-- --     let prog = unsafePerformIO . P.readProgram $ "examples/gcd.asm"
-- --         steps = 15
-- --         x = SConst 2
-- --         y = SConst 3
-- --         -- x = SAny 0
-- --         -- y = SAny 1
-- --         mem = initialiseMemory [(0, x), (1, y)]
-- --         initialState = boot prog mem
-- --         trace = runModel steps initialState
-- --     putStrLn $ Tree.drawTree $ fmap show $ trace

-- -- gcdExampleSMT :: IO ()
-- -- gcdExampleSMT = do
-- --     let prog = unsafePerformIO . P.readProgram $ "examples/gcd.asm"
-- --         steps = 20
-- --         -- x = SConst 2
-- --         -- y = SConst 3
-- --         x = SAny 0
-- --         y = SAny 1
-- --         mem = initialiseMemory [(0, x), (1, y)]
-- --         initialState = boot prog mem
-- --         trace = runModel steps initialState
-- --     -- putStrLn $ Tree.drawTree $ fmap show $ trace
-- --     s <- solveSym trace
-- --     putStrLn $ Tree.drawTree $ fmap renderSolvedState s

-- assemble :: [Instruction] -> Program
-- assemble = zip [0..] . map (\i -> encode i)

-- -- addExampleSMT :: IO ()
-- -- addExampleSMT = do
-- --     let prog = assemble $ [ Instruction $ Load R0 0
-- --                           , Instruction $ Add  R0 1
-- --                           , Instruction $ Halt
-- --                           ]
-- --         steps = 10
-- --         -- x = SConst 2 -- SAny 0
-- --         -- y = SConst 3 -- SAny 1
-- --         x = SAny 0
-- --         y = SAny 1
-- --         mem = initialiseMemory [(0, x), (1, y)]
-- --         initialState = boot prog mem
-- --         -- s = appendConstraints [ SLt x (SConst 10), SLt y (SConst 10)
-- --         --                       , SGt x (SConst 0), SGt y (SConst 0)
-- --         --                       ] initialState
-- --         trace = runModel steps initialState
-- --     s <- solveSym (noZero trace)
-- --     putStrLn $ Tree.drawTree $ fmap renderSolvedState s

-- -- overflow :: Trace -> Trace
-- -- overflow (Tree.Node state children) =
-- --     let cs = pathConstraintList state
-- --         state' = state {pathConstraintList = SNot (overflowNotSet state) : cs}
-- --     in Tree.Node state' (overflow <$> children)

-- -- noZero :: Trace -> Trace
-- -- noZero (Tree.Node state children) =
-- --     let cs = pathConstraintList state
-- --         state' = state {pathConstraintList = zeroNotSet state : cs}
-- --     in Tree.Node state' (noZero <$> children)

-- -- zeroNotSet :: SymState -> Sym
-- -- zeroNotSet s = (SEq ((Map.!) (flags s) Zero) (SConst 0))

-- -- overflowNotSet :: SymState -> Sym
-- -- overflowNotSet s = SEq ((Map.!) (flags s) Overflow) (SConst 0)

-- -- subExampleSMT :: IO ()
-- -- subExampleSMT = do
-- --     let prog = assemble $ [ Instruction $ Load R0 0
-- --                           , Instruction $ Sub  R0 1
-- --                           , Instruction $ Halt
-- --                           ]
-- --         steps = 10
-- --         -- x = SConst 2 -- SAny 0
-- --         -- y = SConst 3 -- SAny 1
-- --         x = SAny 0
-- --         y = SAny 1
-- --         mem = initialiseMemory [(0, x), (1, y)]
-- --         initialState = boot prog mem
-- --         s = appendConstraints [ x `SGt` 20, x `SLt` 30
-- --                               , y `SGt` 0, y `SLt` 10
-- --                               ] initialState
-- --         trace = runModel steps s
-- --     s <- solveSym (noZero trace)
-- --     putStrLn $ Tree.drawTree $ fmap renderSolvedState s
