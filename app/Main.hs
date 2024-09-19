module Main where

import Data.Char (chr, isAlpha, isLower, ord, toLower, toUpper)
import Rewriter
import Word

main :: IO ()
main = do
  putStrLn "Enter a braid word using alphabetical characters.\nCapital for inverse, lowercase for not."
  input <- getLine
  handleInput input

handleInput :: String -> IO ()
handleInput str =
  let word = stringToWord str
   in let (rWord, _) = reverseWord word
       in do
            -- putStrLn "Grid is:"
            -- print grid
            putStrLn "Reversed word is:"
            putStrLn $ showWord rWord
            print rWord

showWord :: BraidWord -> String
showWord [] = []
showWord ((Nothing, _) : rest) = showWord rest
showWord ((Just l, e) : rest) = ((if e then id else toUpper) . chr $ (fromIntegral l + ord 'a')) : showWord rest

stringToWord :: String -> BraidWord
stringToWord str = map (\x -> (Just $ fromIntegral (ord (toLower x) - ord 'a'), isLower x)) $ filter isAlpha str
