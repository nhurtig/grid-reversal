-- This module sets up what reversal rules are
module Rewrites where

import Grid
import Numeric.Natural

type Rewrite = Grid -> Maybe Grid

-- Attempts to apply a reversal rule
-- on the given index starting from the bottom
-- left corner and taking the "outside", intermediary
-- route (right if you can, o/w up)
rewrite :: Natural -> Rewrite
rewrite i g = do
  newG <- rewriteHelper i g
  Just $ simplify newG -- see bottom of file for simplify

-- Does the above function, but doesn't worry about
-- simplification
rewriteHelper :: Natural -> Rewrite
rewriteHelper 0 (v, edges) = do
  (v1, gen1) <- nextInter edges v
  (v2, gen2) <- nextInter edges v1
  makeRewrite gen1 gen2 edges v v2
rewriteHelper i (v, edges) = do
  -- go to next vertex
  nextV <- fst <$> nextInter edges v
  -- do the work, ignoring the passed-up vertex
  (_, newEdges) <- rewriteHelper (i - 1) (nextV, edges)
  -- add the new edges, using the passed-up vertex
  Just (v, newEdges)

-- | x - y |
metric :: Natural -> Natural -> Natural
metric x y
  | x > y = metric y x
  | x < y = y - x
  | otherwise = 0

-- If the generators do rewrite, returns a face that points to
-- the given vertex out of the vertices; otherwise Nothing
makeRewrite :: Generator -> Generator -> Edges -> Vertex -> Vertex -> Maybe Grid
makeRewrite (Just l1, False) (Just l2, True)
  | l1 == l2 = ((Just .) .) . makeCancelFace
  | metric l1 l2 == 1 = ((Just .) .) . makeYBFace l1 l2
  | otherwise = ((Just .) .) . makeSwapFace l1 l2
makeRewrite _ _ = \_ _ _ -> Nothing -- Has to be neg, then pos! (aka up, then right)

--
-- FIRST RULE (sigma cancel)
--

-- Creates a square cell. Given a bottom left corner which is already
-- connected above to a not-given top left corner which is already
-- connected right to a given top right corner. Returns the grid at the
-- bottom left corner of the cell. Given are also the labels of
-- the bottom and right sides of the square.
makeSquare :: Label -> Label -> Edges -> Vertex -> Vertex -> Grid
makeSquare lBot lRig edges botLef topRig =
  let botRig = rightVertex botLef
   in let newEdges = [(botLef, Rig, lBot, botRig), (botRig, Up, lRig, topRig)]
       in (botLef, unionEdges newEdges edges)

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeCancelFace :: Edges -> Vertex -> Vertex -> Grid
makeCancelFace = makeSquare Nothing Nothing

--
-- SECOND RULE (sigma swap)
--

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeSwapFace :: Natural -> Natural -> Edges -> Vertex -> Vertex -> Grid
makeSwapFace lLef lTop = makeSquare (Just lTop) $ Just lLef

--
-- THIRD RULE (yang-baxter)
--

-- Similar to makeSquare, but now puts two edges on the bottom and right each.
-- Labels are same order, bot left to top right.
makeHexagon :: Label -> Label -> Label -> Label -> Edges -> Vertex -> Vertex -> Grid
makeHexagon lBotLef lBotRig lRigBot lRigTop edges botLef topRig =
  let midBot = rightVertex botLef
   in let botRig = rightVertex midBot
       in let midRig = upVertex botRig
           in let newEdges = [(botLef, Rig, lBotLef, midBot), (midBot, Rig, lBotRig, botRig), (botRig, Up, lRigBot, midRig), (midRig, Up, lRigTop, topRig)]
               in (botLef, unionEdges newEdges edges)

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeYBFace :: Natural -> Natural -> Edges -> Vertex -> Vertex -> Grid
makeYBFace lLef lTop = makeHexagon (Just lTop) (Just lLef) (Just lTop) (Just lLef)

--
-- SIMPLIFY
--

-- Moves epsilons around until it can't any more, then returns
simplify :: Grid -> Grid
simplify = trySimplify 0

trySimplify :: Natural -> Grid -> Grid
trySimplify i g
  | i >= gridLength g = g -- done!
  | otherwise = case trySimplifySpot i g of
      Nothing -> trySimplify (i + 1) g
      Just newG -> simplify newG

-- Analogous to rewriteHelper
trySimplifySpot :: Natural -> Rewrite
trySimplifySpot 0 (v, edges) = do
  (v1, gen1) <- nextInter edges v
  (v2, gen2) <- nextInter edges v1
  makeSimplify gen1 gen2 edges v v2
trySimplifySpot i (v, edges) = do
  -- go to next vertex
  nextV <- fst <$> nextInter edges v
  -- do the work, ignoring the passed-up vertex
  (_, newEdges) <- trySimplifySpot (i - 1) (nextV, edges)
  -- add the new edges, using the passed-up vertex
  Just (v, newEdges)

-- Like makeRewrite
makeSimplify :: Generator -> Generator -> Edges -> Vertex -> Vertex -> Maybe Grid
makeSimplify (Nothing, False) (l2, True) -- corner starting w/ epsilon
  =
  ((Just .) .) . makeMoveDown l2
makeSimplify (Just l1, False) (Nothing, True) -- corner ending w/ epsilon
  =
  ((Just .) .) . makeMoveRight (Just l1)
makeSimplify _ _ = \_ _ _ -> Nothing -- Has to be neg, then pos! (aka up, then right)

makeMoveDown :: Label -> Edges -> Vertex -> Vertex -> Grid
makeMoveDown lBot = makeSquare lBot Nothing

makeMoveRight :: Label -> Edges -> Vertex -> Vertex -> Grid
makeMoveRight = makeSquare Nothing
