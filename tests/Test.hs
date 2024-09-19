module Main where
import Test.HUnit
import qualified System.Exit as Exit
import Word
import Rewriter
import WordReversal (reverseString)
import System.Random (mkStdGen)
import RandomWord (testManyWords)
import Control.Monad (when)

testBBAAbbaa :: Test
testBBAAbbaa = TestCase (assertEqual "should be abbbaBAAAB" "abbbaBAAAB" (showWord $ fst $ reverseWord $ stringToWord "BBAAbbaa"))

testBBAAbbaaPaired :: Test
testBBAAbbaaPaired = TestCase (assertEqual "reversal methods should match" (reverseString "BBAAbbaa") (showWord $ fst $ reverseWord $ stringToWord "BBAAbbaa"))

gen = mkStdGen 290
(manyRandom, gen') = testManyWords 100 10 5 gen

tests :: Test
tests = TestList ([TestLabel "testBBAAbbaa" testBBAAbbaa, testBBAAbbaaPaired] ++ manyRandom)


main :: IO ()
main = do
    print tests
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
