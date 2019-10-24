{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Machine.Examples.Common where

import qualified Data.SBV as SBV
import           Control.Selective
import           Machine.Decode
import qualified Machine.SMT as SMT
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import qualified Data.Map.Strict as Map

-- | Register mnemonics
r0, r1, r2, r3 :: Register
[r0, r1, r2, r3] = [R0, R1, R2, R3]

readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

-- | Quick-and-dirty program parser.
--   Comments start with the '#' character.
--   Blank lines are ignored.
parseProgram :: String -> Program
parseProgram src =
    let instructions = map read . removeBlankLines . removeComments . lines $ src
    in addInstructionAddresses instructions
    where removeComments = map (takeWhile (/= '#'))
          removeBlankLines = filter (not . null)
          addInstructionAddresses = zip [0..]

showProgram :: Program -> String
showProgram prog = let is = map snd $ prog
                   in unlines . map show $ is

overflowSet :: State -> Sym Bool
overflowSet s = (Map.!) (flags s) Overflow

halted :: State -> Sym Bool
halted s = (Map.!) (flags s) Halted

allSym :: [Sym Bool] -> Sym Bool
allSym = foldr SAnd (SConst True)

anySym :: [Sym Bool] -> Sym Bool
anySym = foldr SOr (SConst False)

collect :: (State -> Sym Bool) -> Path (Node State) -> Sym Bool
collect predicate path =
    -- foldr SAnd
    let conds = map (predicate . nodeBody) path
    in  anySym conds

-- | Calculate overflow verification condition for a given path in a program.
--
--   All node in a path contain a symbolic boolean representing the state of
--   the @Overflow@ flag after executing the instruction stored in the
--   instruction register. To check if the path is overflow-free, we need to
--   prove the /disjunction/ of overflow conditions on every node of the path.
findOverflowInPath :: Path (Node State) -> Sym Bool
findOverflowInPath = anySym . map (overflow . nodeBody)
    where overflow s = (Map.!) (flags s) Overflow

-- | Check if the @Halted@ flag is set in the /last/ node of the path
pathHalts :: Path (Node State) -> Sym Bool
pathHalts = halted . nodeBody . last

-- | Calculate the amount of jump instructions in the path
countJumps :: Path (Node State) -> Int
countJumps = length . filter isJump
    where isJump :: Node State -> Bool
          isJump (Node _ state) =
            case decode (instructionRegister state) of
                (Instruction (Jump   _)) -> True
                (Instruction (JumpCt _)) -> True
                (Instruction (JumpCf _)) -> True
                _                        -> False

endOfPath :: Path (Node s) -> s
endOfPath = nodeBody . last

isSat :: SBV.SatResult -> Bool
isSat = \case
    (SBV.SatResult (SBV.Satisfiable _ _)) -> True
    _                                     -> False

-- | The path constraint on the final state of the path will be an
--  accumulation of all relevant path constraints
-- pathConstraint :: Path (Node State) -> Sym Bool
-- pathConstraint path =
--     allSym $ map snd . pathConstraintList . nodeBody . last $ path

pathConstraint :: State -> Sym Bool
pathConstraint =
    allSym . map snd . pathConstraintList

-- | Ask the SMT solver if the path is dead, i.e. are the path constraints
--   are unsatisfiable, considering the preconditions.
isDead :: (State -> Sym Bool) -> Path (Node State) -> IO Bool
isDead stateSpace path =
    let finalState = nodeBody (last path)
    in not . isSat <$>
       SBV.satWith SMT.prover
         (SMT.toSMT [pathConstraint finalState `SAnd` stateSpace finalState])

solvePath :: Path (Node State)
          -> (State -> Sym Bool)
          -> (State -> Sym Bool)
          -> (State -> Sym Bool)
          -> IO String
solvePath path pre inv post =
    if (null $ path) then
        error "Empty path!" -- pure ("Empty path", SBV.Unsatisfiable)
    else do
        let start  = nodeBody . head $ path
            finish = nodeBody . last $ path
        putStrLn "Checking preconditions..."
        let precondition = pre start
        preconditionSat <- SBV.satWith SMT.prover
            (SMT.toSMT [pathConstraint start `SAnd` pre start])
        -- Report if preconditions are unsatisfiable and exit
        if (not . isSat $ preconditionSat) then
            pure "Preconditions do not hold" -- , preconditionSat)
        else do
    -- Otherwise -- check the invariant @inv@ in every intermediate state, if
    -- the are any
            putStrLn "Checking the invariant..."
            nodeSats <- filter isSat <$> mapM (solveNode (const precondition) inv) path
            print nodeSats
            pure ""
            -- case nodeSats of
            --     []     -> pure "Path is safe" -- , preconditionSat)
            --     (x:xs) -> pure "Path is not safe" -- , x)



solveNode :: (State -> Sym Bool)
          -> (State -> Sym Bool)
          -> Node State
          -> IO SBV.SatResult
solveNode stateSpace inv (Node nodeId state) = do
    putStrLn $ show nodeId <> ": "
    let prop = stateSpace state     `SAnd`
            --    pathConstraint state `SAnd`
               (SNot $ inv state)
    putStrLn $ "PathConstr: " <> show (pathConstraint state)
    putStrLn $ "Prop: " <> (show $ SNot $ inv state)
    s <- SBV.satWith SMT.prover
            (SMT.toSMT [prop])
    print s
    pure s