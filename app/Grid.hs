{-# LANGUAGE DuplicateRecordFields #-}

module Grid where

import Numeric.Natural

type Label = Natural

-- If we start from the bottom left corner and only take steps
-- right or up, we are guaranteed to be able to read the original
-- word and any intermediate word before it's fully reversed
data Vertex = Vertex {up :: Maybe VEdge, right :: Maybe HEdge} deriving (Eq, Show)

data HEdge = HEdge {next :: Vertex, label :: Label} deriving (Eq, Show)

data VEdge = VEdge {next :: Vertex, label :: Label} deriving (Eq, Show)

data Grid = EmptyGrid | FullGrid {bottomLeftVertex :: Vertex} deriving (Eq, Show)
