-- This module does the work: it reverses words and emits grids.
module Rewriter where

import Grid (Grid, HEdge (HEdge), VEdge (VEdge), Vertex (Vertex))
import Numeric.Natural
import Rewrites (nextInter, rewrite)
import Word

wordToGrid :: BraidWord -> Grid
wordToGrid [] = Vertex Nothing Nothing
wordToGrid ((l, e) : rest) =
  if e
    then Vertex Nothing $ Just $ HEdge (wordToGrid rest) $ Just l
    else Vertex (Just $ VEdge (wordToGrid rest) $ Just l) Nothing

gridToWord :: Grid -> BraidWord
gridToWord v = case nextInter v of
  Nothing -> []
  Just (nextV, mGen) ->
    case mGen of
      Just gen -> gen : gridToWord nextV
      Nothing -> gridToWord nextV

-- The main function!
reverseWord :: BraidWord -> (BraidWord, Grid)
reverseWord w =
  let g = reverseGrid $ wordToGrid w
   in (gridToWord g, g)

gridLength :: Grid -> Natural
gridLength v = case nextInter v of
  Nothing -> 0
  Just (nextV, _) -> 1 + gridLength nextV

reverseGrid :: Grid -> Grid
reverseGrid v = tryReverseGrid v 0

-- Tries to reverse the grid at the given index.
-- If fail, increment. If larger than length, done!
-- If succeed, reset.
tryReverseGrid :: Grid -> Natural -> Grid
tryReverseGrid g i
  | i >= gridLength g = g
  | otherwise = case rewrite i g of
      Nothing -> tryReverseGrid g $ i + 1
      Just newG -> reverseGrid newG
