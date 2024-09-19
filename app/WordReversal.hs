module WordReversal where

import Rewrites
import Word

-- This module is the control for testing.
-- It reverses words directly without grids.
-- It's not meant to be pretty.

-- This assumes there aren't any epsilons
-- in the braid word. If not, invalid results
-- abound.
reverseWordDirect :: BraidWord -> BraidWord
reverseWordDirect w = maybe w reverseWordDirect (rWDHelper w)

rWDHelper :: BraidWord -> Maybe BraidWord
rWDHelper [] = Nothing -- nothing to do
rWDHelper ((Just l1, False) : (Just l2, True) : rest) = case metric l1 l2 of
  0 -> Just rest -- deleted those, try again
  1 -> Just $ [(Just l2, True), (Just l1, True), (Just l2, False), (Just l1, False)] ++ rest
  _ -> Just $ [(Just l2, True), (Just l1, False)] ++ rest
rWDHelper (g : rest) = do
  newRest <- rWDHelper rest
  Just $ g : newRest
