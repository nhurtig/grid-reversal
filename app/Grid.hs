{-# LANGUAGE DuplicateRecordFields #-}

-- This module sets up the basic definitions of the objects
module Grid where

import Numeric.Natural

type Label = Maybe Natural

type Generator = (Label, Bool) -- bool is positive when sigma is positive;
-- Nothing represents epsilon

-- Vertices are just numbers that represent them
type Vertex = Natural

-- create a new vertex to the right of given one
rightVertex :: Vertex -> Vertex
rightVertex n = 2 * n + 1

-- create a new vertex to the above given one
upVertex :: Vertex -> Vertex
upVertex n = 2 * n

-- If we start from the bottom left corner and only take steps
-- right or up, we are guaranteed to be able to read the original
-- word and any intermediate word before it's fully reversed
data Direction = Rig | Up deriving (Eq, Show)

type Edges = Vertex -> Direction -> Maybe (Vertex, Label)

-- adds the given edges to the Edges, replacing existing ones
unionEdges :: [(Vertex, Direction, Label, Vertex)] -> Edges -> Edges
unionEdges [] edges v d = edges v d
unionEdges ((v1, dir, l, v2) : rest) edges v d
  | v1 == v && dir == d = Just (v2, l)
  | otherwise = unionEdges rest edges v d

edgeNext :: Edges -> Vertex -> Direction -> Maybe EdgeInfo
edgeNext edges v d = do
  (newV, l) <- edges v d
  Just
    ( newV,
      ( l,
        case d of
          Up -> False
          Rig -> True
      )
    )

-- Visits the next vertex in the intermediate word (go right if possible,
-- if not go up, if not give up, it's the end
nextInter :: Edges -> Vertex -> Maybe EdgeInfo
nextInter edges v = case edgeNext edges v Rig of
  Just info -> Just info
  Nothing -> edgeNext edges v Up

type EdgeInfo = (Vertex, Generator)

-- Grids are really just the bottom-left corner of themselves
-- and some edges
type Grid = (Vertex, Edges)

-- Length of the intermediate word, including epsilons
gridLength :: Grid -> Natural
gridLength (v, edges) = case nextInter edges v of
  Nothing -> 0
  Just (nextV, _) -> 1 + gridLength (nextV, edges)
