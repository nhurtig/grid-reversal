{-# LANGUAGE DuplicateRecordFields #-}

-- This module sets up the basic definitions of the objects
module Grid where

import Numeric.Natural

type Label = Maybe Natural

type Generator = (Natural, Bool) -- bool is positive when sigma is positive

-- If we start from the bottom left corner and only take steps
-- right or up, we are guaranteed to be able to read the original
-- word and any intermediate word before it's fully reversed
data Vertex = Vertex {up :: Maybe VEdge, right :: Maybe HEdge} deriving (Eq, Show)

data HEdge = HEdge {next :: Vertex, label :: Label} deriving (Eq, Show)

data VEdge = VEdge {next :: Vertex, label :: Label} deriving (Eq, Show)

type EdgeInfo = (Vertex, Maybe Generator)

class Edge a where
  info :: a -> EdgeInfo
  info a = (nxt a, gen a)
  nxt :: a -> Vertex
  gen :: a -> Maybe Generator

instance Edge HEdge where
  nxt (HEdge n _) = n
  gen (HEdge _ l) = do
    index <- l
    Just (index, True)

instance Edge VEdge where
  nxt (VEdge n _) = n
  gen (VEdge _ l) = do
    index <- l
    Just (index, True)

-- Grids are really just the bottom-left corner of themselves
type Grid = Vertex
