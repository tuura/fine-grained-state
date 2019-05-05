module Machine where

import           System.IO.Unsafe (unsafePerformIO)
import           Control.Selective
import           Data.Foldable (sequenceA_)
-- import qualified Data.Tree as Tree
import qualified Data.SBV.Dynamic as SBV
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import           Machine.Semantics
import           Machine.Symbolic
import qualified Machine.SMT      as SMT
import qualified Data.Map.Strict  as Map
import           Machine.Encode
import Machine.Examples.Add
import Machine.Examples.GCD