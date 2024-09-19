module RandomWord where
import Numeric.Natural
import System.Random (mkStdGen, randomR, StdGen)
import Data.Char (chr, toUpper, toLower)
import Test.HUnit
import WordReversal (reverseString)
import Word (stringToWord, showWord)
import Rewriter (reverseWord)

randomChar :: Natural -> StdGen -> (Char, StdGen)
randomChar n gen =
    let (charCode, gen') = randomR (0 :: Int, fromIntegral n) gen -- Random number for a-('a' + n)
        char = chr (fromIntegral charCode + fromEnum 'a')   -- Convert to a letter
        (upperOrLower, gen'') = randomR (0 :: Int, 1) gen'  -- Randomly decide upper/lowercase
    in (if upperOrLower == 0 then toUpper char else toLower char, gen'')

-- length, # strands
randomString :: Natural -> Natural -> StdGen -> (String, StdGen)
randomString 0 _ gen = ("", gen)
randomString n m gen =
    let (char, gen') = randomChar m gen
        (restOfString, gen'') = randomString (n - 1) m gen'
    in (char : restOfString, gen'')

testWord :: String -> Test
testWord str = TestLabel str $ TestCase (assertEqual ("reversal methods should match on " ++ str) (reverseString str) (showWord $ fst $ reverseWord $ stringToWord str))

-- # of tests, length of words, # strands
testManyWords :: Natural -> Natural -> Natural -> StdGen -> ([Test], StdGen)
testManyWords 0 _ _ g = ([], g)
testManyWords count wordLength strandCount gen = let (str, gen') = randomString wordLength strandCount gen in
    let test = testWord str in
        let (rest, gen'') = testManyWords (count - 1) wordLength strandCount gen' in
            (test:rest, gen'')
