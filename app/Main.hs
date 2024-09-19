module Main where

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
