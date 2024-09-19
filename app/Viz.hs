-- This module (poorly) visualizes grids by
-- converting them to graphviz.
module Viz (toViz) where

import Data.Char (chr, ord)
import Grid

toViz :: Grid -> String
toViz (v, edges) = "digraph G {\nlabel=\"right/left=blue, up/down=red, start (leftmost) is bottom-left corner\";\nrankdir=LR;\nnode [style=invis,width=0,height=0,label=\"\"];\nsplines=false;\n" ++ breadthFirstViz [] [v] [] edges ++ "}\n"

breadthFirstViz :: [Vertex] -> [Vertex] -> [Vertex] -> Edges -> String
breadthFirstViz visited (v : rest) toVisit edges
  | v `elem` visited = breadthFirstViz visited rest toVisit edges
  | otherwise = showVertex v edges ++ breadthFirstViz (v : visited) rest (successors v edges ++ toVisit) edges
breadthFirstViz _ [] [] _ = [] -- done!
breadthFirstViz visited [] toVisit edges = breadthFirstViz visited toVisit [] edges

showVertex :: Vertex -> Edges -> String
showVertex v edges =
  show v ++ ";\n" ++ showEdge v (edgeNext edges v Up) ++ showEdge v (edgeNext edges v Rig)

showEdge :: Vertex -> Maybe EdgeInfo -> String
showEdge _ Nothing = ""
showEdge v (Just (next, gen)) = show v ++ " -> " ++ show next ++ " [" ++ vizGen gen ++ "];\n"

vizGen :: Generator -> String
vizGen (label, pos) = "label=\"" ++ vizLabel label ++ "\", color=\"" ++ vizPos pos ++ "\""

vizLabel :: Label -> String
vizLabel (Just l) = [chr $ fromIntegral l + ord 'a']
vizLabel Nothing = "ε"

vizPos :: Bool -> String
vizPos pos = if pos then "blue" else "red"

successors :: Vertex -> Edges -> [Vertex]
successors v edges = successor v edges Up ++ successor v edges Rig

successor :: Vertex -> Edges -> Direction -> [Vertex]
successor v edges dir = case edgeNext edges v dir of
  Nothing -> []
  Just (suc, _) -> [suc]
