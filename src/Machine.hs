module Machine where

import           Control.Selective
import           Data.Foldable        (sequenceA_)
import           System.IO.Unsafe     (unsafePerformIO)
-- import qualified Data.Tree as Tree
import qualified Data.Map.Strict      as Map
import qualified Data.SBV.Dynamic     as SBV
import           Machine.Encode
import           Machine.Examples.Add
import           Machine.Examples.GCD
import           Machine.Semantics
import qualified Machine.SMT          as SMT
import           Machine.Symbolic
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
