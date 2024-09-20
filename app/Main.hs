module Main where

import Rewriter
import System.Process (callProcess)
import Viz (toViz)
import Word (showWord, stringToWord)
import WordReversal (reverseString)
import CompletedGrid

main :: IO ()
main = do
  putStrLn "Enter a braid word using alphabetical characters.\nCapital for inverse, lowercase for not."
  input <- getLine
  handleInput input

handleInput :: String -> IO ()
handleInput str =
  let word = stringToWord str
   in let (rWord, g) = reverseWord word
       in let name = (str ++ "_" ++ showWord rWord)
           in let filename = name ++ ".dot"
               in let pdfFilename = name ++ ".pdf"
                   in do
                        -- Write to the file
                        writeFile filename (toViz g)

                        -- Call dot to generate a PDF
                        tryDot filename pdfFilename

                        -- case exitCode of
                        putStrLn $ "Attempted to generate PDF at: " ++ pdfFilename
                        --   ExitFailure _ -> putStrLn "Failed to generate PDF -- you might not have graphviz."

                        putStrLn $ "Grid graphviz is written to: " ++ filename
                        putStrLn "Direct reversed word is:"
                        putStrLn $ reverseString str
                        putStrLn "Grid reversed word is:"
                        putStrLn $ showWord rWord
                        print $ complete g

tryDot :: FilePath -> FilePath -> IO ()
tryDot dotFile pdfFile = do
  callProcess "dot" ["-Tpdf", dotFile, "-o", pdfFile]

