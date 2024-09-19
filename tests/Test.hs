module Main where
import Test.HUnit
import qualified System.Exit as Exit
import Word
import Rewriter
 
testBBAAbbaa :: Test
testBBAAbbaa = TestCase (assertEqual "should be abbbaBAAAB" "abbbaBAAAB" (showWord $ fst $ reverseWord $ stringToWord "BBAAbbaa"))
 
tests :: Test
tests = TestList [TestLabel "testBBAAbbaa" testBBAAbbaa]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
