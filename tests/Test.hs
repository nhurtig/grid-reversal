module Test where
import Test.HUnit
import qualified System.Exit as Exit
import Main
import Rewriter
 
testBBAAbbaa :: Test
testBBAAbbaa = TestCase (assertEqual "should be baaabABBBA" (showWord $ fst $ reverseWord $ stringToWord "BBAAbbaa") "baaabABBBA")
 
tests :: Test
tests = TestList [TestLabel "testBBAAbbaa" testBBAAbbaa]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
