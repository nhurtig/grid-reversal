module UndirectedGrid where

import Grid
import Prelude hiding (Left, Right)

data Side = Top | Bottom | Left | Right deriving (Eq, Show)

-- counterclockwise rotation
pos :: Side -> Side
pos Top = Left
pos Left = Bottom
pos Bottom = Right
pos Right = Top

neg :: Side -> Side
neg = pos . pos . pos

type UndirectedEdges = Vertex -> Side -> Maybe (Vertex, Label)

type UndirectedGrid = (Vertex, UndirectedEdges)

undirectGrid :: Grid -> UndirectedGrid
undirectGrid (v, e) = (v, undirect (v, e))

undirect :: Grid -> UndirectedEdges
undirect (v, e) = case e v Up of
  Nothing -> undirectRight (v, e)
  Just (vNext, l) -> unionEdges [(v, Top, l, vNext), (vNext, Bottom, l, v)] $ combineEdges (undirectRight (v, e)) $ undirect (vNext, e)

-- assume the Up edge isn't there
undirectRight :: Grid -> UndirectedEdges
undirectRight (v, e) = case e v Rig of
  Nothing -> \_ _ -> Nothing
  Just (vNext, l) -> unionEdges [(v, Right, l, vNext), (vNext, Left, l, v)] $ undirect (vNext, e)

direct :: UndirectedEdges -> Edges
direct v e d = case d of
  Up -> v e Top
  Rig -> v e Right
