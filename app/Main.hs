module Main where

import Data.Char (chr, isAlpha, isLower, ord, toLower, toUpper)
import Grid (Generator)
import Rewriter (reverseWord)
import Word (BraidWord)

main :: IO ()
main = do
  putStrLn "Enter a braid word using alphabetical characters.\nCapital for inverse, lowercase for not."
  input <- getLine
  handleInput input

handleInput :: String -> IO ()
handleInput str =
  let word = map (\x -> (fromIntegral (ord (toLower x) - ord 'a'), isLower x)) $ filter isAlpha str
   in let (rWord, grid) = reverseWord word in
    do
        putStrLn "Reversed word is:"
        putStrLn $ showWord rWord
        putStrLn "Grid is:"
        print grid
        putStrLn "Reversed word is:"
        putStrLn $ showWord rWord
        print rWord

showWord :: BraidWord -> String
showWord = map showGen

showGen :: Generator -> Char
showGen (i, pos) = (if pos then id else toUpper) . chr $ (fromIntegral i + ord 'a')
