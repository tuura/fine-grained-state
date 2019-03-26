{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             TypeFamilies,
             GADTs, DataKinds
              #-}

module FS (Monad, Key(..), Read, Write, FS, dependencies, Semantics
) where

import Prelude hiding (Read, Monad)
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

-- | A type class for keys, equipped with an associated type family that
-- can be used to determine the type of value corresponding to the key.
class Key k where
    -- | The name of the key. Useful for avoiding heterogeneous lists of keys.
    showKey :: k a -> String

-- | The 'FS' data type is a polymorphic state-transformer metalanguage
--   for describing the semantics of programming languages.
--
--   The 'FS' is a type constructor 'f', a constraint 'c' and two
--   effectful functions:
--              'read'  of type 'k -> f v',
--              'write' of type 'k -> f v -> f v'
--
--   The whole thing may be though of as a mutable dictionary with keys
--   of type 'k' and values of type 'v'.
--
--   Mind that the second argument of 'write' is a context-enclosed value of type 'f v'. This allows
--   for the usage of constructions like 'write key2 (read key1)' without restriction
--   the type constructor 'f' to be an instance of 'Applicative' (to gain a way of
--   enclosing pure values in 'f'.)
-- type FS c k a = forall f k. (c f, Key k) => (k -> f (Value k)) ->
--                                             (k -> f (Value k) -> f (Value k)) ->
--                                             Maybe (f a)

-- type Read f a  = forall k. Key k => k -> f (Value k)
type Read k f = forall a. k a -> f a

-- type Write f a = forall k. Key k => k -> f (Value k) -> f (Value k)
type Write k f = forall a. k a -> f a -> f a

type FS c k a = forall f. c f => Read k f -> Write k f -> f a

-- | Calculate data dependencies of a semantic computation
--   The computation must have only static dependencies, hence the
--   'Applicative' constraint. In case of presence of non-static dependecies
--   'Nothing' is returned.
-- dependencies :: FS Selective a -> ([String], [String])
-- dependencies task =
--     partitionEithers . getConst $
--     task trackingRead trackingWrite
--   where trackingRead  k    = Const [Left (showKey k)]
--         trackingWrite k fv = fv *> Const [Right (showKey k)]

dependencies :: Key k => FS Selective k a -> ([String], [String])
dependencies task =
    partitionEithers . getConst $
    task trackingRead trackingWrite

-- trackingRead :: k a -> Const [Either (k a) b1] b2
trackingRead k    = Const [Left (showKey k)]

trackingWrite k fv = fv *> Const [Right (showKey k)]

-- graph :: Ord k => (k -> ([k], [k])) -> k -> Graph k
-- graph deps key = transpose $ overlays [ star k (deps k) | k <- keys Set.empty [key] ]
--   where
--     keys seen []   = Set.toList seen
--     keys seen (x:xs)
--         | x `Set.member` seen = keys seen xs
--         | otherwise           = keys (Set.insert x seen) (deps x ++ xs)

-- draw :: FS Selective v -> String -> String
-- draw computation = exportAsIs . graph deps
--   where
--     deps k = maybe ([], []) dependencies $ computation k

--------------------------------------------------------------------------------
type Semantics c k v a = forall f. c f => (k -> f v) ->
                                          (k -> f v -> f ()) ->
                                          Maybe (f a)