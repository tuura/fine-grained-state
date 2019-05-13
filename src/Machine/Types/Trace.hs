module Machine.Types.Trace where

import Data.Word (Word16)
import qualified Data.Tree as Tree
import           Machine.Types
import           Machine.Types.State
import           Machine.Decode
import           Machine.Encode

type NodeId = Integer

data Node s = Node { nodeId     :: NodeId
                   , nodeBody   :: s
                   } deriving Functor

instance Eq (Node s) where
    (Node x _) == (Node y _) = x == y

instance Ord (Node s) where
    (Node x _) <= (Node y _) = x <= y

instance Show (Node s) where
    show (Node nId _) = show nId

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

subsetTrace :: (s -> Bool) -> Trace s -> [Node s]
subsetTrace property (Trace tree) =
    foldMap (\s -> if property (nodeBody s) then [s] else []) tree

getSatStates :: Trace SolvedState -> [Node SolvedState]
getSatStates = subsetTrace isSatState

type Path s = [s]

-- | Enumerate all paths in a rose tree
paths :: Tree.Tree a -> [Path a]
paths = \case
    (Tree.Node payload []) -> [[payload]]
    (Tree.Node payload xs) -> concat [map (payload:) (paths t) | t <- xs]

exampleTree :: Tree.Tree Int
exampleTree =
    Tree.Node 1
        [ Tree.Node 2 [ Tree.Node 21 []
                      , Tree.Node 22 []
                      , Tree.Node 23 []
                      ]
        , Tree.Node 3 [ Tree.Node 31 []
                      , Tree.Node 32 []
                      ]
         ]

