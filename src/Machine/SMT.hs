module Machine.SMT where

import qualified Data.Map.Strict as Map
import qualified Data.Tree       as Tree
import qualified Data.Set        as Set
import qualified Data.SBV        as SBV
import           Data.Monoid
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans (liftIO)
import           Data.Bool (bool)
import           Data.Int (Int16, Int64)
import           Machine.Decode
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Symbolic

-- | Walk the constraint gathering up the free variables.
gatherFree :: Sym a -> Set.Set (Sym Value)
gatherFree c@(SAny _) = Set.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SSub l r) = gatherFree l <> gatherFree r
gatherFree (SMul l r) = gatherFree l <> gatherFree r
gatherFree (SDiv l r) = gatherFree l <> gatherFree r
gatherFree (SMod l r) = gatherFree l <> gatherFree r
gatherFree (SAbs l)   = gatherFree l
gatherFree (SNot c)   = gatherFree c
gatherFree (SOr l r)  = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r)  = gatherFree l <> gatherFree r
gatherFree (SGt l r)  = gatherFree l <> gatherFree r
gatherFree (SLt l r)  = gatherFree l <> gatherFree r
gatherFree (SConst _) = mempty

-- | Create existential SVals for each of SAny's in the input.
createSym :: [Sym Value] -> SBV.Symbolic (Map.Map Int SBV.SInt64)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $ Map.fromList pairs
    where createSymPair :: Sym Value -> SBV.Symbolic (Int, SBV.SInt64)
          createSymPair (SAny i) = do
            v <- SBV.sInt64 (valName i)
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
    ToSBV Value = SBV.SBV Int64
    ToSBV Bool  = SBV.SBV Bool
    ToSBV a     = SBV.SBV a

-- | Translate symbolic values into the SBV representation
symToSMT :: SBV.SymWord a => Map.Map Int SBV.SInt64 -> Sym a -> SBV.Symbolic (ToSBV a)
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
symToSMT m (SMul l r) =
  (*) <$> symToSMT m l <*> symToSMT m r
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

-- | Solve the path constraints in a symbolic execution state
solveSym :: State -> IO SolvedState
solveSym state = do
    let smtExpr = toSMT . map snd $ pathConstraintList state
    SBV.SatResult smtRes <- SBV.satWith prover smtExpr
    pure (SolvedState state smtRes)

-- | Traverse a symbolic execution trace and solve path constraints in every node
solveTrace :: Trace State -> IO (Trace SolvedState)
solveTrace = traverse solveSym

conjoin :: [SBV.SBool] -> SBV.SBool
conjoin = SBV.bAnd

valName :: Int -> String
valName i = "val_" <> (show i)

prover :: SBV.SMTConfig
prover = SBV.z3 { SBV.verbose = True
                , SBV.redirectVerbose = Just "log.smt2"
                , SBV.printBase = 16
                }