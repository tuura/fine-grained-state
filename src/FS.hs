{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances #-}

module FS (FS, dependencies) where

import Data.Either (partitionEithers)
import Data.Functor.Const

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
type FS c k v a = forall f. c f => (k -> f v) ->
                                   (k -> f v -> f v) ->
                                   Maybe (f a)

-- | Calculate data dependencies of a semantic computation
--   The computation must have only static dependencies, hence the
--   'Applicative' constraint. In case of presence of non-static dependecies
--   'Nothing' is returned.
dependencies :: FS Applicative k v a
             -> Maybe ([k], [k])
dependencies task =
    partitionEithers . getConst <$>
    task trackingRead trackingWrite
  where trackingRead  k    = Const [Left k]
        trackingWrite k fv = fv *> Const [Right k]