{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             TypeFamilies #-}

module FS (FS
, dependencies
) where

import Prelude hiding (Read)
import Control.Selective hiding (dependencies)
import Data.Either (partitionEithers)
import Data.Functor.Const
import Algebra.Graph
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set

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
type Read f k v a = k -> f v

type Write f k v a = k -> f v -> f v

type FS c k v a = forall f. c f => Read f k v a -> Write f k v a -> f a

-- | Calculate data dependencies of a semantic computation
--   The computation must have only static dependencies, hence the
--   'Selective' constraint.
dependencies :: FS Selective k v a -> ([k], [k])
dependencies task =
    partitionEithers . getConst $
    task trackingRead trackingWrite
    where
        trackingRead  k    = Const [Left k]
        trackingWrite k fv = fv *> Const [Right k]
