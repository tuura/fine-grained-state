{-# Language UndecidableInstances #-}

module Machine.Symbolic where

import           Control.Monad (ap)
import           Control.Applicative (Alternative, (<|>))
import           Data.Functor (void)
import           ListT
import           Control.Selective
import           Control.Monad.State.Class
import           Control.Monad.IO.Class
import           Data.IORef
import           Control.Monad.State (evalState)
import qualified Control.Monad.State as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Tree       as Tree
import           Machine.Types
import qualified Machine.SMT as SMT
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Types.OneOrTwo
import           Machine.Encode
import           Machine.Decode
import           Machine.Semantics

--------------------------------------------------------------------------------
---------------- Symbolic Engine -----------------------------------------------
--------------------------------------------------------------------------------
instance MonadState s m => MonadState s (ListT m) where
    get   = Monad.lift get
    put   = Monad.lift . put
    state = Monad.lift . state

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
-- newtype SymEngine a = SymEngine
--     { runSymEngine :: Monad.StateT State OneOrTwo a }
--     deriving (Functor, Applicative, Prelude.Monad, MonadState State)
newtype SymEngine a = SymEngine
    { runSymEngine :: Monad.StateT State (ListT IO) a }
    deriving (Functor, Applicative, Alternative, Prelude.Monad, MonadIO, MonadState State)

instance Selective SymEngine where
    select = selectM

symEngineConstraint :: Sym Bool -> SymEngine ()
symEngineConstraint constr = do
    modify (appendConstraints [("", constr)])

--------------------------------------------------------------------------------
--- Selective-like combinators for the symbolic execution engine ---------------
--------------------------------------------------------------------------------

-- | Conditionally perform a computation.
whenSym :: SymEngine (Sym Bool) -> SymEngine () -> SymEngine ()
whenSym condition computation = do
    -- Evaluate the condition
    newPathConstraint <- condition
    pathConstraints <- map snd . pathConstraintList <$> get
    -- Check the condition is trivial, i.e. doesn't contain any free variables
    case (tryFoldConstant newPathConstraint) of
        -- Perform the computation if the condition trivially holds
        SConst True  -> computation
        -- Do nothing if it trivially doesn't hold
        SConst False -> pure ()
        -- Finally, branch if the condition is a non-trivial symbolic expression
        _ -> do
            -- let satResult1 = liftIO $ SMT.satBool (SMT.toSMT (newPathConstraint:pathConstraints))
            --     satResult2 = liftIO $ SMT.satBool (SMT.toSMT ((SNot newPathConstraint):pathConstraints))
            -- in -- Add the path constraint to the context and execute the computation
            --    (whenS (SMT.isSat <$> satResult1) $ symEngineConstraint newPathConstraint *> computation)
            --    <|>
            --    -- Alternatively, add the negated path constraint and do nothing
            --    (whenS (SMT.isSat <$> satResult2) $ symEngineConstraint (SNot newPathConstraint))
            satResult1 <- liftIO $ SMT.satBool (SMT.toSMT (newPathConstraint:pathConstraints))
            satResult2 <- liftIO $ SMT.satBool (SMT.toSMT ((SNot newPathConstraint):pathConstraints))
            liftIO $ putStrLn $ "Solving: " <> show (newPathConstraint:pathConstraints)
            liftIO $ print satResult1
            let p = whenS (SMT.isSat <$> pure satResult1) $ symEngineConstraint newPathConstraint *> computation
                q = whenS (SMT.isSat <$> pure satResult2) $ symEngineConstraint (SNot newPathConstraint)
            -- Add the path constraint to the context and execute the computation
            p <|> q
            -- Alternatively, add the negated path constraint and do nothing

-- | The semantics for @JumpZero@ for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpZeroSym :: SImm8 -> SymEngine ()
jumpZeroSym simm =
    whenSym (readKey (F Zero))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

-- | The semantics for @JumpCt@ for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpCtSym :: SImm8 -> SymEngine ()
jumpCtSym simm =
    whenSym (readKey (F Condition))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

-- | The semantics for @JumpCf@ for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpCfSym :: SImm8 -> SymEngine ()
jumpCfSym simm =
    whenSym (SNot <$> readKey (F Condition))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

-- | The semantics for @loadMI@ for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
loadMISym :: Register -> MemoryAddress -> SymEngine ()
loadMISym rX dmemaddr = do
    writeRegister rX =<< readMemory =<< toMemoryAddress <$> readMemory dmemaddr
    where toMemoryAddress :: Sym Value -> MemoryAddress
          toMemoryAddress val = case (getValue val) of
            (Just val) ->
                Machine.Decode.fromBitsLE (Prelude.take 16 $ Machine.Decode.blastLE val)
            _ -> error "loadMISym: can't perform indirect memory load with a symbolic pointer"

-- | Perform one step of symbolic execution
-- symStep :: State -> OneOrTwo State
symStep :: State -> IO [State]
symStep state = do
    (instrCode, fetched) <- pipeline state
    let instrSemantics =
             case decode instrCode of
                (Instruction (JumpZero offset)) -> jumpZeroSym offset
                (Instruction (LoadMI reg addr)) -> loadMISym reg addr
                (Instruction (JumpCt offset))   -> jumpCtSym offset
                (Instruction (JumpCf offset))   -> jumpCfSym offset
                i                               -> instructionSemantics i readKey writeKey
    t <- ListT.toList $ Monad.runStateT (runSymEngine instrSemantics) fetched
    pure $ map snd t

pipeline :: State -> IO (InstructionCode, State)
pipeline state =
    let steps = do fetchInstruction
                   incrementInstructionCounter
                   readInstructionRegister
    in do
        t <- ListT.toList $ Monad.runStateT (runSymEngine steps) state
        case t of
          [result] -> pure result
          _ -> error
                 "piplineStep: impossible happened: fetchInstruction returned not a singleton."

runModelImpl :: IORef NodeId -> Int -> State -> IO (Trace State)
runModelImpl nodeIdRef steps state = do
    modifyIORef nodeIdRef (+ 1)
    nodeId <- readIORef nodeIdRef
    let halted = (Map.!) (flags state) Halted == SConst True
    if | steps <= 0 -> pure (mkTrace (Node nodeId state) [])
       | otherwise  -> if halted then pure (mkTrace (Node nodeId state) [])
                                 else do newStates <- liftIO $ symStep state
                                         children <- Prelude.traverse
                                            (runModelImpl nodeIdRef (steps - 1)) newStates
                                         pure $ mkTrace (Node nodeId state) (children)

runModel :: Int -> State -> IO (Trace State)
runModel steps state = do
    nodeIdRef <- newIORef (0 :: NodeId)
    runModelImpl nodeIdRef steps state

-- | Instance of the Machine.Metalanguage read command for symbolic execution
readKey :: Key a -> SymEngine (Sym a)
readKey = \case
    Reg  src  -> (Map.!) <$> (registers <$> get) <*> pure src
    Addr src  -> (Map.!) <$> (memory    <$> get) <*> pure src
    F    flag -> (Map.!) <$> (flags     <$> get) <*> pure flag
    IC        -> SConst . instructionCounter  <$> get
    IR        -> SConst . instructionRegister <$> get
    Prog addr -> SConst <$> snd <$> ((!!) <$> (program <$> get) <*> pure (fromIntegral addr))

-- | Instance of the Machine.Metalanguage write command for symbolic execution
writeKey :: Key a
         -> SymEngine (Sym a)
         -> SymEngine (Sym a)
writeKey k v = case k of
    Reg  target -> v >>= (\x -> writeRegister target x *> pure x)
    Addr target -> v >>= (\x -> writeMemory   target x *> pure x)
    F    flag   -> v >>= (\x -> writeFlag flag x *> pure x)
    IC          -> v >>= \x -> case getValue x of
        (Just val) -> (modify $ \s -> s {instructionCounter = val}) *> pure (SConst val)
        _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IC is not supported"
    IR        -> v >>= \case
        (SConst val) -> writeInstructionRegister val *> pure (SConst val)
        _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IR is not supported"
    Prog _    -> error "Machine.Semantics.Symbolic: Can't write Program"

-- | Increment the instruction counter.
incrementInstructionCounter :: SymEngine ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = (+) 1 (instructionCounter currentState)}

fetchInstruction :: SymEngine ()
fetchInstruction =
    snd <$> ((!!) <$> (program <$> get) <*> (fromIntegral . instructionCounter <$> get)) >>=
    writeInstructionRegister

readInstructionRegister :: SymEngine InstructionCode
readInstructionRegister = instructionRegister <$> get

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Sym Value -> SymEngine ()
writeRegister target value =
    modify $ \s ->
        s {registers = Map.adjust (const value) target (registers s)}

readMemory :: MemoryAddress -> SymEngine (Sym Value)
readMemory src = (Map.!) <$> (memory    <$> get) <*> pure src

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> Sym Value -> SymEngine ()
writeMemory target value =
    modify $ \s ->
        s {memory = Map.adjust (const value) (fromIntegral target) (memory s)}

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Sym Bool -> SymEngine ()
writeFlag flag value = do
    modify $ \s ->
        s { flags = Map.adjust (const value) flag (flags s) }

writeInstructionRegister :: InstructionCode -> SymEngine ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}