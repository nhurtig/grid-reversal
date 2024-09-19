module Main where

import Data.Char (isAlpha, isLower, ord, toLower)
import Rewriter (reverseWord)

main :: IO ()
main = do
  putStrLn "Enter a braid word using alphabetical characters.\nCapital for inverse, lowercase for not."
  input <- getLine
  handleInput input

handleInput :: String -> IO ()
handleInput str = let word = map (\x -> (fromIntegral (ord (toLower x) - ord 'a'), isLower x)) $ filter isAlpha str in do
    print word
    print $ reverseWord word
