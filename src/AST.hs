{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances,
             TypeFamilies,
             GADTs, StandaloneDeriving, LambdaCase
              #-}

module AST where

import Prelude hiding (Read, Monad)
import qualified Prelude (Monad)
import Control.Selective hiding (dependencies)
import Data.Either (partitionEithers)
import Data.Functor.Const
import Algebra.Graph
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set

data AST k v where
    Read   :: k -> AST k v
    Write  :: k -> AST k v -> AST k v
    Fmap   :: (a -> b) -> AST k a -> AST k b
    Pure   :: a -> AST k a
    Ap     :: AST k (a -> b) -> AST k a -> AST k b
    Select :: AST k (Either a b) -> AST k (a -> b) -> AST k b
    Bind   :: AST k a -> (a -> AST k b) -> AST k b

instance (Show k) => Show (AST k v) where
    show = \case
        Read k -> "read (" ++ show k ++ ")"
        Write k fv -> "write (" ++ show k ++ ") (" ++ show fv ++ ")"
        Fmap  f x  -> "f <$> " ++ show x
        Pure x -> "pure x"
        Ap ff x -> "ff <*> " ++ show x
        Select ff x -> "ff <*? " ++ show x
        Bind x ff -> show x ++ " >>= ff"

instance Functor (AST k) where
    -- fmap = Fmap
    fmap f x = Fmap f x

instance Applicative (AST k) where
    pure = Pure
    (<*>) = Ap

instance Selective (AST k) where
    select = Select

instance Prelude.Monad (AST k) where
    (>>=) = Bind