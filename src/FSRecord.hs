{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             TypeFamilies,
             GADTs, DataKinds
              #-}

module FSRecord where

import Prelude hiding (Read, readIO, Monad)
import qualified Prelude (Monad)
import Control.Selective hiding (dependencies)
import Data.Either (partitionEithers)
import Data.Functor.Const
import Algebra.Graph
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- -- | A type class for keys, equipped with an associated type family that
-- -- can be used to determine the type of value corresponding to the key.
-- class Key k where
--     -- | The name of the key. Useful for avoiding heterogeneous lists of keys.
--     showKey :: k a -> String

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