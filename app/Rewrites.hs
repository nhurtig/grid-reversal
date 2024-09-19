-- This module sets up what reversal rules are
module Rewrites where

import Grid
import Numeric.Natural

type Rewrite = Grid -> Maybe Grid

type RewriteInfoHandler = Generator -> Generator -> Edges -> Vertex -> Vertex -> Maybe Grid

-- | x - y |
metric :: Natural -> Natural -> Natural
metric x y
  | x > y = metric y x
  | x < y = y - x
  | otherwise = 0

-- If the generators do rewrite, returns a grid starting with
-- the first given vertex with the added rewrite; otherwise Nothing
handleRewrite :: RewriteInfoHandler
handleRewrite (Just l1, False) (Just l2, True)
  | l1 == l2 = ((Just .) .) . makeCancelFace
  | metric l1 l2 == 1 = ((Just .) .) . makeYBFace l1 l2
  | otherwise = ((Just .) .) . makeSwapFace l1 l2
handleRewrite _ _ = \_ _ _ -> Nothing -- Has to be neg, then pos! (aka up, then right)

-- Do the rewrite, and if it works, fully simplify
handleRewriteThenSimplify :: RewriteInfoHandler
handleRewriteThenSimplify g1 g2 edges v1 v2 = handleRewrite g1 g2 edges v1 v2 >>= Just . simplify

simplify :: Grid -> Grid
simplify = makeRewriter handleSimplify

--
-- General functions
--

-- Does the Natural->Rewrite function as much
-- as it can
rewriteMany :: (Natural -> Rewrite) -> Grid -> Grid
rewriteMany = tryRewrite 0

-- helper for rewriteMany
tryRewrite :: Natural -> (Natural -> Rewrite) -> Grid -> Grid
tryRewrite i r g
  | i >= gridLength g = g -- done!
  | otherwise = case r i g of
      Nothing -> tryRewrite (i + 1) r g -- keep scanning
      Just newG -> rewriteMany r newG -- restart at 0

extractRewriteInfo :: RewriteInfoHandler -> Natural -> Rewrite
extractRewriteInfo handler 0 (v, edges) = do
  (v1, gen1) <- nextInter edges v
  (v2, gen2) <- nextInter edges v1
  handler gen1 gen2 edges v v2
extractRewriteInfo handler i (v, edges) = do
  -- go to next vertex
  nextV <- fst <$> nextInter edges v
  -- do the work, ignoring the passed-up vertex
  (_, newEdges) <- extractRewriteInfo handler (i - 1) (nextV, edges)
  -- add the new edges, using the passed-up vertex
  Just (v, newEdges)

-- Easy helper function
makeRewriter :: RewriteInfoHandler -> Grid -> Grid
makeRewriter handler = rewriteMany $ extractRewriteInfo handler

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

-- Like handleRewrite
handleSimplify :: RewriteInfoHandler
-- corner starting w/ epsilon
handleSimplify (Nothing, False) (l2, True) =
  ((Just .) .) . makeMoveDown l2
-- corner ending w/ epsilon
handleSimplify (Just l1, False) (Nothing, True) =
  ((Just .) .) . makeMoveRight (Just l1)
handleSimplify _ _ = \_ _ _ -> Nothing -- Doesn't match form

makeMoveDown :: Label -> Edges -> Vertex -> Vertex -> Grid
makeMoveDown lBot = makeSquare lBot Nothing

makeMoveRight :: Label -> Edges -> Vertex -> Vertex -> Grid
makeMoveRight = makeSquare Nothing
