module Main where
import Test.HUnit
import qualified System.Exit as Exit
import Word
import Rewriter
import WordReversal (reverseString)
import System.Random (mkStdGen)
import RandomWord
import Control.Monad (when)

testBBAAbbaa :: Test
testBBAAbbaa = TestCase (assertEqual "should be abbbaBAAAB" "abbbaBAAAB" (showWord $ fst $ reverseWord $ stringToWord "BBAAbbaa"))

testBBAAbbaaPaired :: Test
testBBAAbbaaPaired = TestCase (assertEqual "reversal methods should match" (reverseString "BBAAbbaa") (showWord $ fst $ reverseWord $ stringToWord "BBAAbbaa"))

testFoo = let w = [(Just 3,False),(Just 5,False),(Just 2,False),(Just 4,True),(Just 0,False),(Just 2,True),(Just 1,False),(Just 5,True),(Just 0,True),(Just 2,False)] in
    testWordComplete w

gen = mkStdGen 290
(manyRandom, gen') = testManyWords 100 10 5 gen

(manyComplete, gen'') = testManyWordsComplete 100 10 5 gen'

tests :: Test
tests = TestList ([TestLabel "testBBAAbbaa" testBBAAbbaa, testBBAAbbaaPaired, testFoo] ++ manyComplete ++ manyRandom)


main :: IO ()
main = do
    print tests
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
