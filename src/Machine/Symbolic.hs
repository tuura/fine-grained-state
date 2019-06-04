{-# Language UndecidableInstances #-}

module Machine.Symbolic where

import           Control.Monad (ap)
import           Control.Applicative (Alternative, (<|>))
import           Data.Functor (void)
import           ListT
import           Control.Selective
import           Control.Monad.State.Class
import           Control.Monad.State (evalState)
import qualified Control.Monad.State as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Tree       as Tree
import           Machine.Types
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
    { runSymEngine :: Monad.StateT State [] a }
    deriving (Functor, Applicative, Alternative, Prelude.Monad, MonadState State)

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
    pathConstraint <- condition
    -- Check the condition is trivial, i.e. doesn't contain any free variables
    case (tryFoldConstant pathConstraint) of
        -- Perform the computation if the condition trivially holds
        SConst True  -> computation
        -- Do nothing if it trivially doesn't hold
        SConst False -> pure ()
        -- Finally, branch if the condition is a non-trivial symbolic expression
        _ ->
            -- Add the path constraint to the context and execute the computation
            (symEngineConstraint pathConstraint *> computation)
            <|>
            -- Alternatively, add the negated path constraint and do nothing
            symEngineConstraint (SNot pathConstraint)


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
symStep :: State -> [State]
symStep state =
    let (instrCode, fetched) = pipeline state
        instrSemantics =
             case decode instrCode of
                (Instruction (JumpZero offset)) -> jumpZeroSym offset
                (Instruction (LoadMI reg addr)) -> loadMISym reg addr
                (Instruction (JumpCt offset))   -> jumpCtSym offset
                (Instruction (JumpCf offset))   -> jumpCfSym offset
                i                               -> instructionSemantics i readKey writeKey
    in map snd $ Monad.runStateT (runSymEngine instrSemantics) fetched

pipeline :: State -> (InstructionCode, State)
pipeline state =
    let steps = do fetchInstruction
                   incrementInstructionCounter
                   readInstructionRegister
    in case Monad.runStateT (runSymEngine steps) state of
            [result] -> result
            _ -> error
                "piplineStep: impossible happened: fetchInstruction returned not a singleton."

runModelM :: MonadState NodeId m => Int -> State -> m (Trace State)
runModelM steps state = do
    modify (+ 1)
    nodeId <- get
    let halted    = (Map.!) (flags state) Halted == SConst True
        newStates = symStep state
    if | steps <= 0 -> pure (mkTrace (Node nodeId state) [])
       | otherwise  -> if halted then pure (mkTrace (Node nodeId state) [])
                                 else do children <- Prelude.traverse (runModelM (steps - 1)) newStates
                                         pure $ mkTrace (Node nodeId state) (children)

runModel :: Int -> State -> Trace State
runModel steps state = evalState (runModelM steps state) 0

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