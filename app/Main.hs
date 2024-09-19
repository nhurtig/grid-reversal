module Main where

import Rewriter
import Viz (toViz)
import Word (showWord, stringToWord)
import WordReversal (reverseString)

main :: IO ()
main = do
  putStrLn "Enter a braid word using alphabetical characters.\nCapital for inverse, lowercase for not."
  input <- getLine
  handleInput input

handleInput :: String -> IO ()
handleInput str =
  let word = stringToWord str
   in let (rWord, g) = reverseWord word
       in do
            putStrLn "Grid graphviz is:"
            putStrLn $ toViz g
            putStrLn "Direct reversed word is:"
            putStrLn $ reverseString str
            putStrLn "Grid reversed word is:"
            putStrLn $ showWord rWord
