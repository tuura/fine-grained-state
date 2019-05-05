module Machine.SMT where

import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Data.Set as Set
import Data.Monoid
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
-- import qualified Data.SBV.Dynamic as SBV
import qualified Data.SBV as SBV
import Data.SBV (constrain, SBool, (.<))
import Data.Bool (bool)
import Data.Int (Int16)
import Data.Typeable


import Machine.Decode
import Machine.Types
import Machine.Symbolic

-- | Walk the constraint gathering up the free variables.
gatherFree :: Sym a -> Set.Set (Sym Value)
gatherFree c@(SAny _) = Set.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SSub l r) = gatherFree l <> gatherFree r
gatherFree (SDiv l r) = gatherFree l <> gatherFree r
gatherFree (SMod l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r)  = gatherFree l <> gatherFree r
gatherFree (SAbs l)   = gatherFree l
gatherFree (SNot c)   = gatherFree c
gatherFree (SOr l r)  = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SGt l r)  = gatherFree l <> gatherFree r
gatherFree (SLt l r)  = gatherFree l <> gatherFree r
gatherFree (SConst _) = mempty

-- | Create existential SVals for each of SAny's in the input.
createSym :: [Sym Value] -> SBV.Symbolic (Map.Map Int SBV.SInt16)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair :: Sym Value -> SBV.Symbolic (Int, SBV.SInt16)
          createSymPair (SAny i) = do
            v <- SBV.sInt16 (valName i)
            pure (i, v)
          createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a symbolic value the SMT solver can solve.
--   Each constraint in the list is conjoined with the others.
toSMT :: [Sym Bool] -> SBV.Symbolic SBV.SBool
toSMT cs = do
  let freeVars = gatherFree (foldr SAnd (SConst True) cs)
  sValMap <- createSym (Set.toList freeVars)
  smts <- traverse (symToSMT sValMap) cs
  pure $ conjoin smts

-- | Translate type indices of the Sym GADT into SBV phantom types
type family ToSBV a where
    ToSBV Value = SBV.SBV Int16
    ToSBV Bool  = SBV.SBV Bool
    ToSBV a     = SBV.SBV a

-- | Translate symbolic values into the SBV representation
symToSMT :: SBV.SymWord a => Map.Map Int SBV.SInt16 -> Sym a -> SBV.Symbolic (ToSBV a)
symToSMT m (SEq l r) =
  (SBV..==) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SGt l r) =
  (SBV..>) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SLt l r) =
  (SBV..<) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SAdd l r) =
  (+) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SSub l r) =
  (-) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SDiv l r) =
  SBV.sDiv <$> symToSMT m l <*> symToSMT m r
symToSMT m (SMod l r) =
  SBV.sMod <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SConst w) = pure (SBV.literal w)
symToSMT m (SAbs l) =
  abs <$> symToSMT m l
symToSMT m (SNot c) =
  SBV.bnot <$> symToSMT m c
symToSMT m (SAnd l r) =
  (SBV.&&&) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SOr l r) =
  (SBV.|||) <$> symToSMT m l <*> symToSMT m r
symToSMT m (SAny i) =
  case Map.lookup i m of
    Just val -> pure val
    Nothing -> error "Missing symbolic variable."

-- valueToSVal :: Value -> SBV.SVal
-- valueToSVal w = SBV.svInteger (SBV.KBounded True 16) (toInteger w)

-- -- | Unsafely coerce SBV's untyped symbolic value from SWord to SBool
-- sValToSBool :: SBV.SVal -> SBV.SVal
-- sValToSBool w = w `SBV.svNotEqual` (valueToSVal 0)

-- -- | Unsafely coerce SBV's untyped symbolic value from SBool to SWord
-- sValToSWord :: SBV.SVal -> SBV.SVal
-- sValToSWord w = SBV.svIte w (valueToSVal 1) (valueToSVal 0)

data SolvedState = SolvedState SymState SBV.SMTResult

-- | Render the output of the SMT solver into a human-readable form
renderSMTResult :: SBV.SMTResult -> String
renderSMTResult (SBV.Unsatisfiable _ _) = "Unsatisfiable"
renderSMTResult s@(SBV.Satisfiable _ _) =
  let dict = SBV.getModelDictionary s
  in  if Map.null dict then "Trivial" else renderDict dict
renderSMTResult _ = "Error"

renderDict :: Show v => Map.Map String v -> String
renderDict m =
  foldr toStr "" (Map.toList m)
  where toStr (k,v) s = k <> " = " <> show v <> ", " <> s

renderSolvedState :: SolvedState -> String
renderSolvedState (SolvedState state c) =
  "IC: " <> show (instructionCounter state) <> "\n" <>
  "IR: " <> show (decode $ instructionRegister state) <> "\n" <>
  "Flags: " <> show (Map.toList $ flags state) <> "\n" <>
  "Path Constraints: \n" <> renderPathConstraints (pathConstraintList state) <> "\n" <>
  "Solved Values: " <> renderSMTResult c

renderPathConstraints :: [Sym Bool] -> String
renderPathConstraints xs = foldr (\x acc -> "  && " <> show x <> "\n" <> acc) "" xs

solveSym :: Trace -> IO (Tree.Tree SolvedState)
solveSym (Tree.Node state c) = do
    let smtExpr = toSMT $ pathConstraintList state
    SBV.SatResult smtRes <- SBV.satWith prover smtExpr
    children <- traverse solveSym c
    pure $ Tree.Node (SolvedState state smtRes) children

conjoin :: [SBV.SBool] -> SBV.SBool
conjoin = SBV.bAnd -- foldr (SBV.svAnd . sValToSBool) SBV.svTrue

valName :: Int -> String
valName i = "val_" <> (show i)

prover :: SBV.SMTConfig
prover = SBV.z3 { SBV.verbose = True
                , SBV.redirectVerbose = Just "log.smt2"
                , SBV.printBase = 16
                }