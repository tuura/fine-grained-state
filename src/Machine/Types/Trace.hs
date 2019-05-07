module Machine.Types.Trace where

import Data.Word (Word16)
import qualified Data.Tree as Tree
import           Machine.Types
import           Machine.Types.State
import           Machine.Decode
import           Machine.Encode

type NodeId = Word16

data Node s = Node { nodeId     :: NodeId
                   , nodeBody   :: s
                   } deriving Functor

renderNode :: Node State -> String
renderNode node =
  "Node Id: " <> show (nodeId node) <> "\n" <>
  renderState (nodeBody node)

renderSolvedNode :: Node SolvedState -> String
renderSolvedNode node =
  "Node Id: " <> show (nodeId node) <> "\n" <>
  renderSolvedState (nodeBody node)

-- | The symbolic execution trace
newtype Trace s = Trace {unTrace :: Tree.Tree (Node s)}
    deriving Functor

instance Foldable Trace where
    foldMap f (Trace tree) = foldMap (f . nodeBody) tree

instance Traversable Trace where
    traverse f (Trace tree) = Trace <$> traverse (\(Node n s) -> Node n <$> f s) tree

foldConstantsInTrace :: Trace State -> Trace State
foldConstantsInTrace = fmap foldConstantsInState

renderTrace :: Trace State -> String
renderTrace (Trace tree) =
    Tree.drawTree (renderNode <$> tree)

renderSolvedTrace :: Trace SolvedState -> String
renderSolvedTrace (Trace tree) =
    Tree.drawTree (renderSolvedNode <$> tree)

mkTrace :: Node s -> [Trace s] -> Trace s
mkTrace node children = Trace $ Tree.Node node (map unTrace children)

-- | Impose a path constraint on every state in the trace.
--   Useful for checking whole program properties, e.g. the absence of overflow
constraint :: Label -> (State -> Sym Bool) -> Trace State -> Trace State
constraint label constr = fmap (\s -> appendConstraints [(label, constr s)] s)

traceDepth :: Trace s -> Int
traceDepth = length . Tree.flatten . unTrace

subsetTrace :: (s -> Bool) -> Trace s -> [s]
subsetTrace property =
    foldMap (\s -> if property s then [s] else [])
