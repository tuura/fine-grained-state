{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
module Machine.Types.OneOrTwo (
        OneOrTwo (..), collapse, toList
    ) where

-- | A data type which is either a singleton or a pair of values
data OneOrTwo a = One a | Two a a
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative OneOrTwo where
    pure x  = One x
    f <*> x = undefined

instance Monad OneOrTwo where
    x >>= f = case collapse (fmap f x) of
                Just y -> y
                Nothing -> error "impossible happened, more than two branches in tree."

-- | Collapse a nested @OneOrTwo@ into @OneOrTwo@ if its either a @One@ or a
--   @Two@ containing two @One@s. Otherwise return @Nothing@.
collapse :: OneOrTwo (OneOrTwo a) -> Maybe (OneOrTwo a)
collapse = \case
    One x   -> Just x
    Two x y -> case (x, y) of
                 (One p, One q) -> Just (Two p q)
                 _              -> Nothing

-- | Convert a @OneOrTwo@ into a one or two-element list.
toList :: OneOrTwo a -> [a]
toList = \case One x   -> [x]
               Two x y -> [x, y]

ex1 :: OneOrTwo Char
ex1 = Two 'a' 'b'

ex2 :: OneOrTwo Char
ex2 = One 'c'

ex3 :: OneOrTwo (OneOrTwo Char)
ex3 = Two ex2 ex2