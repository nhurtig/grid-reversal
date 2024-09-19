-- This module does the work: it reverses words and emits grids.
module Rewriter where

import Grid
import Numeric.Natural
import Rewrites
import Word

wordToGrid :: BraidWord -> Grid
wordToGrid = wordToGridHelper 1

wordToGridHelper :: Vertex -> BraidWord -> Grid
wordToGridHelper next [] = (next, \_ _ -> Nothing)
wordToGridHelper next ((l, e) : rest) =
  if e -- e true means right, e false means up
    then (next, unionEdges [(next, Rig, l, rightVertex next)] $ snd (wordToGridHelper (rightVertex next) rest))
    else (next, unionEdges [(next, Up, l, upVertex next)] $ snd (wordToGridHelper (upVertex next) rest))

gridToWord :: Grid -> BraidWord
gridToWord (v, edges) = case nextInter edges v of
  Nothing -> [] -- done!
  Just (nextV, gen) ->
    gen : gridToWord (nextV, edges)

-- The main function!
reverseWord :: BraidWord -> (BraidWord, Grid)
reverseWord w =
  let g = reverseGrid $ wordToGrid w
   in (gridToWord g, g)

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
