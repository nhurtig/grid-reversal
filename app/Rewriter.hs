-- This module does the work: it reverses words and emits grids.
module Rewriter where

import Grid
import Rewrites
import Word (BraidWord)

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
    gen : Rewriter.gridToWord (nextV, edges)

-- The main function!
reverseWord :: BraidWord -> (BraidWord, Grid)
reverseWord w =
  let g = reverseGrid $ wordToGrid w
   in (Rewriter.gridToWord g, g)

reverseGrid :: Grid -> Grid
reverseGrid = makeRewriter True handleRewrite
