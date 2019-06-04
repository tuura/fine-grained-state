module Machine.Types.OneOrTwo (
        OneOrTwo (..), collapse
    ) where

-- | A data type which is either a singleton or a pair of values
data OneOrTwo a = One a | Two a a
    deriving (Eq, Show, Functor)

-- | Collapse a nested @OneOrTwo@ into @OneOrTwo@ if its either a @One@ or a
--   @Two@ containing two @One@s. Otherwise return @Nothing@.
collapse :: OneOrTwo (OneOrTwo a) -> Maybe (OneOrTwo a)
collapse = \case
    One x   -> Just x
    Two x y -> case (x, y) of
                 (One p, One q) -> Just (Two p q)
                 _              -> Nothing

ex1 :: OneOrTwo Char
ex1 = Two 'a' 'b'

ex2 :: OneOrTwo Char
ex2 = One 'c'

ex3 :: OneOrTwo (OneOrTwo Char)
ex3 = Two ex2 ex2