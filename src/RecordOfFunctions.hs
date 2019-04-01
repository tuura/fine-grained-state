{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             TypeFamilies,
             GADTs, DataKinds
              #-}

module RecordOfFunctions where

import Prelude hiding (Read, readIO)
import qualified Prelude (Read)
import Control.Selective hiding (dependencies)
import Data.Either (partitionEithers)
import Data.Functor.Const
import Data.Functor (void)
import Algebra.Graph
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
-- type Monad m = (Selective m, Prelude.Monad m)

class Unconstrained (a :: * -> *)
instance Unconstrained a where

-- -- | A type class for keys, equipped with an associated type family that
-- -- can be used to determine the type of value corresponding to the key.
-- class Key k where
--     -- | The name of the key. Useful for avoiding heterogeneous lists of keys.
--     showKey :: k a -> String

{-
    Plain record-of-functions
-}

-- | Simple teletype interface represented as a record-of-functions
data Store f k v = Store { _getItem :: k -> f v
                         , _putItem :: k -> f v -> f ()
                         }

type Ident = String

getIO :: (Show k, Prelude.Read v) => k -> IO v
getIO k = putStr ("Get " ++ show k ++ " = ") >> read <$> getLine

putIO :: (Show k, Show v) => k -> IO v -> IO ()
putIO k fv = do
    x <- fv
    putStr ("Put " ++ show k ++ " = ") >> putStrLn (show x)

storeIO :: Store IO Ident Int
storeIO = Store getIO putIO

echo :: Store f Ident v -> f ()
echo (Store getIdent putIdent) =
    putIdent "y" (getIdent "x")

{-
    Curried record-of-functions
-}

type StoreC f k v = (k -> f v) -> (k -> f v -> f ()) -> f ()

echoC :: Monad f => StoreC f Ident Int
echoC getItem putItem =
    putItem "y" (getItem "x")

{-
    Curried record-of-functions with constrained existential 'f'
-}

type StoreE c k v = forall f. c f => (k -> f v) -> (k -> f v -> f ()) -> f ()

echoE :: StoreE Unconstrained Ident Int
echoE getItem putItem =
    putItem "y" (getItem "x")

{-
    Now we want static analysis
-}

data Dependency k = DepGet k | DepPut k

instance Show k => Show (Dependency k) where
    show (DepGet k) = "Get " ++ show k
    show (DepPut k) = "Put " ++ show k

dependencies :: StoreE Applicative k v -> [Dependency k]
dependencies task =
    getConst $ task trackingGet trackingPut

trackingGet :: k -> Const [Dependency k] a
trackingGet k = Const [DepGet k]

trackingPut :: k -> Const [Dependency k] a -> Const [Dependency k] b
trackingPut k fv = fv *> Const [DepPut k]





-- type Read f a  = forall k. Key k => k -> f (Value k)
type Read k f = forall a. k a -> f a

-- type Write f a = forall k. Key k => k -> f (Value k) -> f (Value k)
type Write k f = forall a. k a -> f a -> f a

type FS c k a = forall f. c f => Read k f -> Write k f -> f a

type Choice k f = forall a. Either (k a) (k a) -> f a -> f a -> f a

data Key :: * -> * where
    Lit   :: Int    -> Key String
    Var   :: String -> Key String

comp :: FS Applicative Key String
comp read write = write (Var "x") (read $ Lit 1)

-- | A 'Get' in 'IO' for GHCi experiments.
readIO :: Read Key IO
readIO (Lit x)   = putStr ("Read : Lit " ++ show x) >> getLine
readIO (Var ident) = putStr ("Read : Var   " ++ show ident ++ " = ") >> getLine

-- | A 'Put' in 'IO' for GHCi experiments.
writeIO :: Write Key IO
writeIO (Lit x) y   = putStr ("Write : Lit " ++ show x ++ " = ") >>
                      (y >>= \val -> putStrLn val *> pure val)
writeIO (Var ident) x = putStr ("Write : Var " ++ show ident ++ " = ") >>
                        (x >>= \val -> putStrLn val *> pure val)

choiceIO :: Choice Key IO
choiceIO (Left  x) onLeft onRight = onLeft
choiceIO (Right y) onLeft onRight = onRight