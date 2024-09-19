-- This module describes what words are (pretty easy!)
module Word (showWord, stringToWord, BraidWord) where

import Data.Char (chr, isAlpha, isLower, ord, toLower, toUpper)
import Grid

type BraidWord = [Generator]

showWord :: BraidWord -> String
showWord [] = []
showWord ((Nothing, _) : rest) = showWord rest
showWord ((Just l, e) : rest) = ((if e then id else toUpper) . chr $ (fromIntegral l + ord 'a')) : showWord rest

stringToWord :: String -> BraidWord
stringToWord str = map (\x -> (Just $ fromIntegral (ord (toLower x) - ord 'a'), isLower x)) $ filter isAlpha str
