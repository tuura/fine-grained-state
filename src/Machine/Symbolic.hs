module Machine.Symbolic where

import           Control.Monad (ap)
import           Data.Functor (void)
import           Control.Selective
import           Control.Monad.State.Class
import           Control.Monad.State (evalState)
import qualified Data.Map.Strict as Map
import qualified Data.Tree       as Tree
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Encode
import           Machine.Decode
import           Machine.Semantics

--------------------------------------------------------------------------------
---------------- Symbolic Engine -----------------------------------------------
--------------------------------------------------------------------------------


-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data SymEngine a = SymEngine
    { runSymEngine :: State -> [(a, State)] }
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
        case (tryFoldConstant evalCond) of
                SConst True -> runSymEngine comp s'
                SConst False -> pure ((), s')
                _ -> do
                    b@(compRes, s'') <- runSymEngine comp s'
                    (f a (snd b))
        -- pure ((), appendConstraints [evalCond] s')
        -- pure ((), appendConstraints [(SNot evalCond)] s'')
    -- concat [f a (snd b) | a <- runSymEngine cond s, b <- runSymEngine comp (snd a)]
    where
        f :: (Sym Bool, State) -> State -> [((), State)]
        f (b, sNoExec) sOnExec =
            [ ((), appendConstraints [("", b)] sOnExec)
            , ((), appendConstraints [("", SNot b)] sNoExec)]

-- -- | Conditionally perform an effect.
-- whenSym :: SymEngine (Sym Bool) -> SymEngine () -> SymEngine ()
-- whenSym cond comp = -- select (bool (Right ()) (Left ()) <$> x) (const <$> y)
--     SymEngine $ \s -> do
--         a@(evalCond, s') <- runSymEngine cond s
--         b@(compRes, s'') <- runSymEngine comp s'
--         (f a (snd b))
--         -- pure ((), appendConstraints [evalCond] s')
--         -- pure ((), appendConstraints [(SNot evalCond)] s'')
--     -- concat [f a (snd b) | a <- runSymEngine cond s, b <- runSymEngine comp (snd a)]
--     where
--         f :: (Sym Bool, State) -> State -> [((), State)]
--         f (b, sNoExec) sOnExec =
--             [ ((), appendConstraints [("", b)] sOnExec)
--             , ((), appendConstraints [("", SNot b)] sNoExec)]

instance Selective SymEngine where
    select = selectM

instance Prelude.Monad SymEngine where
    return a       = SymEngine $ \s -> [(a, s)]
    SymEngine r >>= f = SymEngine $ \s ->
        let outcomes = r s
        in concat $ map (\(result, state) -> runSymEngine (f result) state) outcomes

instance (MonadState State) SymEngine where
    get   = SymEngine $ \s -> [(s, s)]
    put s = SymEngine $ \_ -> [((), s)]

-- | The semantics for JumpZero for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpZeroSym :: SImm8 -> SymEngine ()
jumpZeroSym simm =
    whenSym (readKey (F Zero))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

-- | The semantics for JumpCt for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpCtSym :: SImm8 -> SymEngine ()
jumpCtSym simm =
    whenSym (readKey (F Condition))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

-- | The semantics for JumpCt for now has to be hijacked and implemented in terms of the
--   symbolic-aware whenSym (instead of the desired Selective whenS).
jumpCfSym :: SImm8 -> SymEngine ()
jumpCfSym simm =
    whenSym (SNot <$> readKey (F Condition))
            (void $ writeKey IC ((SAdd (SConst . fromIntegral $ simm)) <$> readKey IC))

loadMISym :: Register -> MemoryAddress -> SymEngine ()
loadMISym rX dmemaddr = do
    writeRegister rX =<< readMemory =<< toMemoryAddress <$> readMemory dmemaddr
    where toMemoryAddress :: Sym Value -> MemoryAddress
          toMemoryAddress val = case (getValue val) of
            (Just val) ->
                Machine.Decode.fromBitsLE (take 16 $ Machine.Decode.blastLE val)
            _ -> error "loadMISym: can't perform indirect memory load with a symbolic pointer"

-- | Perform one step of symbolic execution
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
    in map snd $ runSymEngine instrSemantics fetched

pipeline :: State -> (InstructionCode, State)
pipeline state =
    let steps = do fetchInstruction
                   incrementInstructionCounter
                   readInstructionRegister
    in case runSymEngine steps state of
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
                                 else do children <- traverse (runModelM (steps - 1)) newStates
                                         pure $ mkTrace (Node nodeId state) children

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