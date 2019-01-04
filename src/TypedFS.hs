{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             TypeFamilies #-}

module TypedFS (Key(..), FS, dependencies) where

import Prelude hiding (Read)
import Data.Either (partitionEithers)
import Data.Functor.Const

-- | A type class for keys, equipped with an associated type family that
-- can be used to determine the type of value corresponding to the key.
class Key k where
    type Value k :: *
    -- | The name of the key. Useful for avoiding heterogeneous lists of keys.
    showKey :: k -> String

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

type Read f a = forall k. Key k => k -> f (Value k)

type Write f a = forall k. Key k => k -> f (Value k) -> f (Value k)

type FS c a = forall f. c f => Read f a -> Write f a -> Maybe (f a)

-- type FS c a = forall f. c f => (forall k. Key k => k -> f (Value k)) ->
--                                (forall k. Key k => k -> f (Value k) -> f (Value k)) ->
--                                Maybe (f a)

-- | Calculate data dependencies of a semantic computation
--   The computation must have only static dependencies, hence the
--   'Applicative' constraint. In case of presence of non-static dependecies
--   'Nothing' is returned.
dependencies :: FS Applicative a -> Maybe ([String], [String])
dependencies task =
    partitionEithers . getConst <$>
    task trackingRead trackingWrite
  where trackingRead  k    = Const [Left (showKey k)]
        trackingWrite k fv = fv *> Const [Right (showKey k)]