{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             GADTs,
             StandaloneDeriving,
             DerivingVia,
             MultiWayIf,
             LambdaCase #-}


-- hs <- read "Main.hs"
-- let (hi, o) = ghc hs
-- write "Main.hi" (pure hi)
-- write "Main.o" (pure o)

module Applications.BuildSystems where

import Prelude hiding (Monad)
import Data.Functor (void)
import Control.Selective
import FS

newtype BuildKey a = BuildKey String

instance Key BuildKey where
    showKey (BuildKey s) = s

sprsh1 :: BuildKey a -> FS Applicative BuildKey Integer
sprsh1 (BuildKey "B1") read write =
    write (BuildKey "B1") ((+) <$> read (BuildKey "A1") <*> read (BuildKey "A2"))
-- sprsh1 "B2" = Task $ \fetch -> ((*2) <$> fetch "B1")

data SourceCode = SourceCode
    deriving (Show, Eq, Enum, Bounded)

data ObjectFile = ObjectFile
    deriving (Show, Eq, Enum, Bounded)

data InterfaceFile = InterfaceFile
    deriving (Show, Eq, Enum, Bounded)

data File a where
    Main    :: File SourceCode
    Main_o  :: File ObjectFile
    Main_hi :: File InterfaceFile

    Lib1    :: File SourceCode
    Lib1_o  :: File ObjectFile
    Lib1_hi :: File InterfaceFile

    Lib2    :: File SourceCode
    Lib2_o  :: File ObjectFile
    Lib2_hi :: File InterfaceFile

ghc :: Selective f => SourceCode -> f (ObjectFile, InterfaceFile)
ghc = undefined

-- build1 :: FS Selective File ()
-- build1 read write = void $
--     (read Main) `bindS` \sc ->
--         ghc sc `bindS` \(o, hi) ->
--             write Main_o (pure o) *>
--             write Main_hi (pure hi)

-- build1 :: FS Monad File ()
-- build1 read write = void $ do
--     -- read Main
--     sc <- read Main
--     (o, hi) <- ghc sc
--     write Main_o  (pure o)
--     write Main_hi (pure hi)
