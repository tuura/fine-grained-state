module Machine.Types.Trace.Viz where

import qualified Algebra.Graph            as G
import qualified Algebra.Graph.Export.Dot as G
import qualified Data.Text.Lazy           as Text
import           Machine.Decode
import           Machine.Types
import           Machine.Types.State
import           Machine.Types.Trace
import qualified Text.Pretty.Simple       as PPrint

type GTrace s = G.Graph (Node s)

mkGTrace :: Trace s -> GTrace s
mkGTrace = G.tree . unTrace

renderDagrejs :: GTrace State -> (String, String)
renderDagrejs trace =
    let vs = G.vertexList trace
        es = G.edgeList   trace
    in (concatMap renderVertex vs, concatMap renderEdge es)

renderVertex :: Node State -> String
renderVertex v@(Node nId s) =
    let label       = mkVertexLabel v
        description = renderState s
    in show label <> ": { \n" <>
       "    description: `<pre>" <> description <> "</pre>`\n" <>
       "},"

renderEdge :: (Node State, Node State) -> String
renderEdge (s, t) = "g.setEdge(\"" <> mkVertexLabel s <> "\", \"" <>
                                      mkVertexLabel t <> "\" ,{ });\n"

mkVertexLabel :: Node State -> String
mkVertexLabel (Node nId s) =
    let ic = instructionCounter s
    in  show (nId, decode . snd $ program s !! (fromIntegral ic))
