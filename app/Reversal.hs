-- This module sets up what reversal rules are
module Reversal where

import Grid
import Numeric.Natural

type Rewrite = Grid -> Maybe Grid

-- Visits the next vertex in the intermediate word (go right if possible,
-- if not go up, if not give up, it's the end
nextInter :: Vertex -> Maybe EdgeInfo
nextInter (Vertex _ (Just e)) = Just $ info e
nextInter (Vertex (Just e) Nothing) = Just $ info e
nextInter (Vertex Nothing Nothing) = Nothing

-- Attempts to apply the first reversal rule
-- on the given index starting from the bottom
-- left corner and taking the "outside", intermediary
-- route (right if you can, o/w up)
sigmaCancel :: Natural -> Rewrite
sigmaCancel i (Grid v) = do
  newV <- vertexCancel i v
  Just $ Grid newV

-- Does the above function, but on a vertex instead of a grid; returns
-- the new vertex
vertexCancel :: Natural -> Vertex -> Maybe Vertex
vertexCancel 0 v = do
  (v1, mGen1) <- nextInter v
  (v2, mGen2) <- nextInter v1
  gen1 <- mGen1
  gen2 <- mGen2
  makeVertexCancel gen1 gen2 v v2
vertexCancel i v = do
  -- go to next vertex
  nextV <- fst <$> nextInter v
  vertexCancel (i - 1) nextV

-- If the generators do cancel, returns a face that points to
-- the given vertex out of the vertices; otherwise Nothing
makeVertexCancel :: Generator -> Generator -> Vertex -> Vertex -> Maybe Vertex
makeVertexCancel (l1, False) (l2, True)
  | l1 /= l2 = \_ _ -> Nothing -- indices don't match!
  | l1 == l2 = (Just .) . makeVertexCancelFace
makeVertexCancel _ _ = \_ _ -> Nothing -- Has to be neg, then pos!

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeVertexCancelFace :: Vertex -> Vertex -> Vertex
makeVertexCancelFace oldBotLef oldTopRig =
  let botRight = Vertex (Just $ VEdge oldTopRig Nothing) Nothing
   in Vertex (up oldBotLef) (Just $ HEdge botRight Nothing)
