-- This is the definition that we'd like to use;
-- Grid is a partial grid
{-# LANGUAGE TupleSections #-}
module CompletedGrid where

import Grid
import Numeric.Natural
import Rewrites
import UndirectedGrid
import Word
import Prelude hiding (Either, Left, Right)

data CompletedGrid
  = SigmaSwap {leftLabel :: Natural, topLabel :: Natural} -- first of the 3 rewrite cells
  | YangBaxter {leftLabel :: Natural, topLabel :: Natural}
  | SigmaCancel Natural
  | AllEpsilon -- first of the 3 simplify cells
  | TopBottom Natural -- non epsilons on top, bottom
  | LeftRight Natural -- non epsilons on left, right
  | Vertical {topGrid :: CompletedGrid, bottomGrid :: CompletedGrid}
  | Horizontal {leftGrid :: CompletedGrid, rightGrid :: CompletedGrid}
  deriving (Eq, Show)

-- read along the right, up axes (NOT the right, down axes)
getWord :: CompletedGrid -> Side -> [Label]
getWord (SigmaSwap leftL topL) side
  | side `elem` [Top, Bottom] = [Just topL]
  | otherwise = [Just leftL]
getWord (YangBaxter leftL topL) side = case side of
  Left -> [Just leftL]
  Bottom -> [Just topL, Just leftL]
  Top -> [Just topL]
  Right -> [Just topL, Just leftL]
getWord (SigmaCancel i) side
  | side `elem` [Left, Top] = [Just i]
  | otherwise = [Nothing]
getWord AllEpsilon _ = [Nothing]
getWord (TopBottom i) side
  | side `elem` [Top, Bottom] = [Just i]
  | otherwise = [Nothing]
getWord (LeftRight i) side
  | side `elem` [Left, Right] = [Just i]
  | otherwise = [Nothing]
getWord (Vertical top bottom) side
  | side `elem` [Left, Right] = getWord bottom side ++ getWord top side
  | side == Top = getWord top side
  | otherwise = getWord bottom side
getWord (Horizontal left right) side
  | side `elem` [Top, Bottom] = getWord left side ++ getWord right side
  | side == Left = getWord left side
  | otherwise = getWord right side

-- returns problem statement o.w. Nothing. Input word
-- is the input word (left inverses, top regulars). By
-- unicity, don't need to check the other sides.
validate :: CompletedGrid -> BraidWord -> Maybe String
validate g w = case validGrid g of
  Just err -> Just err
  Nothing ->
    let leftLabels = getWord g Left
        leftWord = map (, False) leftLabels
        topLabels = getWord g Top
        topWord = map (, True) topLabels
        gridWord = leftWord ++ topWord
     in if gridWord == w then Nothing else Just $ "Words don't match!" ++ showWord w ++ " " ++ showWord gridWord

-- checks that all the constraints are correct
validGrid :: CompletedGrid -> Maybe String
validGrid (SigmaSwap i j) = if metric i j >= 2 then Nothing else Just "Swap too close!"
validGrid (YangBaxter i j) = if metric i j == 1 then Nothing else Just "Yang baxter wrong!"
validGrid (Vertical top bottom) = if getWord top Bottom == getWord bottom Top then Nothing else Just $ "Top+bottom don't match!" ++ show (getWord top Bottom) ++ show (getWord bottom Top)
validGrid (Horizontal left right) = if getWord left Right == getWord right Left then Nothing else Just $ "Left+right don't match!" ++ show (getWord left Right) ++ " " ++ show (getWord left Top) ++ show (getWord right Left) ++ show (getWord right Top)
validGrid (SigmaCancel _) = Nothing
validGrid (TopBottom _) = Nothing
validGrid (LeftRight _) = Nothing
validGrid AllEpsilon = Nothing

data Either err ok = OK ok | Err err deriving (Eq, Show)

isOK :: Either a b -> Bool
isOK (OK _) = True
isOK _ = False

complete :: Grid -> Either String CompletedGrid
complete = completeUndirect . undirectGrid

completeUndirect :: UndirectedGrid -> Either String CompletedGrid
completeUndirect (botLeft, e) = case e botLeft Top of
  Nothing -> Err "Bottom left corner doesn't have an up!" -- somehow the bottom left corner doesn't have an up!
  Just (vUp, _) -> case e vUp Top of
    Just _ -> paste Top $ seam e Right vUp -- vUp isn't the top left corner; must be a seam!
    Nothing -> case e vUp Right of -- vUp is the top left corner
      Nothing -> Err "Top left corner doesn't have a right!" -- somehow top left corner doesn't have a right!
      Just (vUpRight, _) -> case e vUpRight Right of
        Nothing -> recognizePrimitive vUp e -- vUpRight is top right corner... MUST be a primitive cell.
        Just _ -> paste Right $ seam e Bottom vUpRight -- vUpRight isn't the top right corner; must be a seam!

-- Completes both grids and pastes them, with the second grid to the
-- Side of the first grid
paste :: Side -> (UndirectedGrid, UndirectedGrid) -> Either String CompletedGrid
paste dir (g1, g2) = case completeUndirect g1 of
  Err e -> Err e
  OK cG1 -> case completeUndirect g2 of
    Err e -> Err e
    OK cG2 -> OK $ case dir of
      Bottom -> Vertical cG1 cG2
      Top -> Vertical cG2 cG1
      Right -> Horizontal cG1 cG2
      Left -> Horizontal cG2 cG1

-- cuts a seam in the direction given
seam :: UndirectedEdges -> Side -> Vertex -> (UndirectedGrid, UndirectedGrid)
seam e = seamHelper e e

seamHelper :: UndirectedEdges -> UndirectedEdges -> Side -> Vertex -> (UndirectedGrid, UndirectedGrid)
seamHelper ePos eNeg dir v =
  let ePos' = removeEdges [(v, pos dir)] ePos
   in let eNeg' = removeEdges [(v, neg dir)] eNeg
       in case ePos v dir of
            Nothing -> (reset v ePos', reset v eNeg') -- reached the end
            Just (newV, _) -> seamHelper ePos' eNeg' dir newV

-- Goes in the direction until it can't anymore
run :: Side -> UndirectedEdges -> Vertex -> Vertex
run dir e v = case e v dir of
  Nothing -> v
  Just (vNext, _) -> run dir e vNext

-- moves to the bottom left corner
reset :: Vertex -> UndirectedEdges -> UndirectedGrid
reset v e = (run Bottom e $ run Left e v, e)

-- Given top left vertex, if this is a primitive then
-- return it else give up
recognizePrimitive :: Vertex -> UndirectedEdges -> Either String CompletedGrid
recognizePrimitive topLeft e = case e topLeft Right of
  Nothing -> Err "topLeft doesn't have a right!"
  Just (topRight, lTop) -> case e topLeft Bottom of
    Nothing -> Err "topLeft doesn't have a bottom!"
    Just (botLeft, lLeft) -> case e botLeft Right of
      Nothing -> Err "botLeft doesn't have a right!"
      Just (botRight1, lBot) -> case e topRight Bottom of
        Nothing -> Err "topRight doesn't have a bottom!"
        Just (botRight2, lRight) ->
          if botRight1 == botRight2
            then recognizeSquare lLeft lTop lRight lBot
            else recognizeHexagon botRight1 botRight2 lLeft lTop lBot lRight e

recognizeSquare :: Label -> Label -> Label -> Label -> Either String CompletedGrid
recognizeSquare Nothing Nothing Nothing Nothing = OK AllEpsilon
recognizeSquare (Just lLeft) Nothing (Just lRight) Nothing
  | lLeft == lRight = OK $ LeftRight lLeft
  | otherwise = Err "LeftRight labels aren't equal!"
recognizeSquare Nothing (Just lTop) Nothing (Just lBot)
  | lTop == lBot = OK $ TopBottom lTop
  | otherwise = Err "TopBottom labels aren't equal!"
recognizeSquare (Just lLeft) (Just lTop) Nothing Nothing
  | lLeft == lTop = OK $ SigmaCancel lLeft
  | otherwise = Err "Sigma cancel labels don't line up!"
recognizeSquare (Just lLeft) (Just lTop) (Just lRight) (Just lBot)
  | lLeft == lRight && lTop == lBot && metric lLeft lTop >= 2 = OK $ SigmaSwap lLeft lTop
  | otherwise = Err "Sigma swap labels don't line up!"
recognizeSquare _ _ _ _ = Err "Square doesn't have right epsilon pattern!"

recognizeHexagon :: Vertex -> Vertex -> Label -> Label -> Label -> Label -> UndirectedEdges -> Either String CompletedGrid
recognizeHexagon botMid midRight (Just lLeft) (Just lTop) (Just lBotLeft) (Just lTopRight) e = case e botMid Right of
  Nothing -> Err "botMid doesn't have a right!"
  Just (_, Nothing) -> Err "lBotRight is epsilon!"
  Just (botRight1, Just lBotRight) -> case e midRight Bottom of
    Nothing -> Err "midRight doesn't have a bottom!"
    Just (_, Nothing) -> Err "lRightBot is epsilon!"
    Just (botRight2, Just lRightBot) ->
      if botRight1 == botRight2 && lLeft == lBotRight && lBotRight == lTopRight && lTop == lBotLeft && lBotLeft == lRightBot && metric lLeft lTop == 1
        then OK $ YangBaxter lLeft lTop
        else Err "YB vertices/labels don't line up!"
recognizeHexagon _ _ _ _ _ _ _ = Err "Hexagon has epsilon labels!"
