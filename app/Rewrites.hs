-- This module sets up what reversal rules are
module Rewrites where

import Grid
import Numeric.Natural
import Debug.Trace (trace)
import Word (BraidWord)

type Rewrite = Grid -> Maybe Grid

-- Visits the next vertex in the intermediate word (go right if possible,
-- if not go up, if not give up, it's the end
nextInter :: Vertex -> Maybe EdgeInfo
nextInter (Vertex _ (Just e)) = Just $ info e
nextInter (Vertex (Just e) Nothing) = Just $ info e
nextInter (Vertex Nothing Nothing) = Nothing

-- Visits the next vertex in the intermediate word; keeps
-- visiting until it actually finds a generator and then stops
nextInterNonEpsilon :: Vertex -> Maybe (Vertex, Generator)
nextInterNonEpsilon (Vertex _ (Just e)) = nonEpsilon $ info e
nextInterNonEpsilon (Vertex (Just e) Nothing) = nonEpsilon $ info e
nextInterNonEpsilon (Vertex Nothing Nothing) = Nothing

nonEpsilon :: EdgeInfo -> Maybe (Vertex, Generator)
nonEpsilon (v, Nothing) = nextInterNonEpsilon v -- keep looking!
nonEpsilon (v, Just g) = Just (v, g) -- found it!

-- Attempts to apply a reversal rule
-- on the given index starting from the bottom
-- left corner and taking the "outside", intermediary
-- route (right if you can, o/w up)
rewrite :: Natural -> Rewrite
rewrite i g
  | trace ("rewrite" ++ show (gridToWord g) ++ " " ++ show i) False = undefined
  | otherwise = do
      newV <- vertexRewrite i g
      Just newV

-- Does the above function, but on a vertex instead of a grid; returns
-- the new vertex if a rewrite was possible
vertexRewrite :: Natural -> Vertex -> Maybe Vertex
vertexRewrite 0 v = do
  (v1, mGen1) <- nextInter v
  gen1 <- mGen1
  (v2, gen2) <- nextInterNonEpsilon v1
  dmakeVertexRewrite gen1 gen2 v v2
vertexRewrite i v = do
  -- go to next vertex
  nextV <- fst <$> nextInter v
  newNextV <- vertexRewrite (i - 1) nextV
  replaceInter v newNextV

dmakeVertexRewrite :: Generator -> Generator -> Vertex -> Vertex -> Maybe Vertex
dmakeVertexRewrite g g2 v v2
  | trace ("dmVR" ++ show (g, g2, v, v2)) False = undefined
  | otherwise = makeVertexRewrite g g2 v v2

-- Replaces the inter branch of this vertex with the new given
-- one.
replaceInter :: Vertex -> Vertex -> Maybe Vertex
replaceInter (Vertex u (Just (HEdge _ l))) newInter = Just $ Vertex u $ Just $ HEdge newInter l
replaceInter (Vertex (Just (VEdge _ l)) Nothing) newInter = Just $ Vertex (Just $ VEdge newInter l) Nothing
replaceInter (Vertex Nothing Nothing) _ = Nothing

-- | x - y |
metric :: Natural -> Natural -> Natural
metric x y
  | x > y = metric y x
  | x < y = y - x
  | otherwise = 0

-- If the generators do rewrite, returns a face that points to
-- the given vertex out of the vertices; otherwise Nothing
makeVertexRewrite :: Generator -> Generator -> Vertex -> Vertex -> Maybe Vertex
makeVertexRewrite (l1, False) (l2, True)
  | trace ("mvr" ++ show (l1, l2)) False = undefined
  | l1 == l2 = (Just .) . makeVertexCancelFace
  | metric l1 l2 == 1 = (Just .) . makeVertexYBFace l1 l2
  | otherwise = (Just .) . makeVertexSwapFace l1 l2
makeVertexRewrite _ _ = \_ _ -> Nothing -- Has to be neg, then pos! (aka up, then right)

--
-- FIRST RULE (sigma cancel)
--

-- Creates a square cell. Given a bottom left corner which is already
-- connected above to a not-given top left corner which is already
-- connected right to a given top right corner. Returns the new
-- bottom left corner of the cell. Given are also the labels of
-- the bottom and right sides of the square.
makeSquare :: Label -> Label -> Vertex -> Vertex -> Vertex
makeSquare lBot lRig oldBotLef oldTopRig =
  let botRig = Vertex (Just $ VEdge oldTopRig lRig) Nothing
   in Vertex (up oldBotLef) (Just $ HEdge botRig lBot)

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeVertexCancelFace :: Vertex -> Vertex -> Vertex
makeVertexCancelFace = makeSquare Nothing Nothing

--
-- SECOND RULE (sigma swap)
--

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeVertexSwapFace :: Natural -> Natural -> Vertex -> Vertex -> Vertex
makeVertexSwapFace lLef lTop = makeSquare (Just lTop) $ Just lLef

--
-- THIRD RULE (yang-baxter)
--

-- Similar to makeSquare, but now puts two edges on the bottom and right each.
-- Labels are same order, bot left to top right.
makeHexagon :: Label -> Label -> Label -> Label -> Vertex -> Vertex -> Vertex
makeHexagon lBotLef lBotRig lRigBot lRigTop oldBotLef oldTopRig =
  let midRig = Vertex (Just $ VEdge oldTopRig lRigTop) Nothing
   in let botRig = Vertex (Just $ VEdge midRig lRigBot) Nothing
       in let botMid = Vertex Nothing (Just $ HEdge botRig lBotRig)
           in Vertex (up oldBotLef) (Just $ HEdge botMid lBotLef)

-- Creates the face from Def 3.2.1 corresponding to sigma cancel, using
-- the given vertices
makeVertexYBFace :: Natural -> Natural -> Vertex -> Vertex -> Vertex
makeVertexYBFace lLef lTop = makeHexagon (Just lTop) (Just lLef) (Just lTop) (Just lLef)

-- TODO: delete duped code
gridToWord :: Grid -> BraidWord
gridToWord v = case nextInter v of
  Nothing -> []
  Just (nextV, mGen) ->
    case mGen of
      Just gen -> gen : gridToWord nextV
      Nothing -> gridToWord nextV
