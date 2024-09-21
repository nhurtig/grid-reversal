module RandomWord where

import Data.Char (chr, toLower, toUpper)
import Numeric.Natural
import Rewriter
import System.Random (StdGen, mkStdGen, randomR)
import Test.HUnit
import Word
import WordReversal
import Data.Maybe
import CompletedGrid

-- Randomly decide upper/lowercase
randomChar :: Natural -> StdGen -> (Char, StdGen)
randomChar n gen =
  let (upperOrLower, gen') = randomR (0 :: Int, 1) gen
   in randomCharCase upperOrLower n gen'

-- 1 if upper, 0 if lower
randomCharCase :: Int -> Natural -> StdGen -> (Char, StdGen)
randomCharCase upperOrLower n gen =
  let (charCode, gen') = randomR (0 :: Int, fromIntegral n) gen -- Random number for a-('a' + n)
      char = chr (fromIntegral charCode + fromEnum 'a') -- Convert to a letter
   in (if upperOrLower == 1 then toUpper char else toLower char, gen')

-- length, # strands
randomStringHelper :: (Natural -> StdGen -> (Char, StdGen)) -> Natural -> Natural -> StdGen -> (String, StdGen)
randomStringHelper _ 0 _ gen = ("", gen)
randomStringHelper charGen n m gen =
  let (char, gen') = charGen m gen
      (restOfString, gen'') = randomStringHelper charGen (n - 1) m gen'
   in (char : restOfString, gen'')

randomString :: Natural -> Natural -> StdGen -> (String, StdGen)
randomString = randomStringHelper randomChar

randomProperWord :: Natural -> Natural -> StdGen -> (BraidWord, StdGen)
randomProperWord wordLen strandCount gen =
  let (numUpper, gen') = randomR (0 :: Int, fromIntegral wordLen) gen
      numUpperNat = fromIntegral numUpper
      (uppers, gen'') = randomStringHelper (randomCharCase 1) numUpperNat strandCount gen'
      (lowers, gen''') = randomStringHelper (randomCharCase 0) (wordLen - numUpperNat) strandCount gen''
   in (makeWord False uppers ++ makeWord True lowers, gen''')

-- if string is empty, makes epsilon
makeWord :: Bool -> String -> BraidWord
makeWord sign [] = [(Nothing, sign)]
makeWord _ str = stringToWord str

testWord :: String -> Test
testWord str = TestLabel str $ TestCase (assertEqual ("reversal methods should match on " ++ str) (reverseString str) (showWord $ fst $ reverseWord $ stringToWord str))

-- # of tests, length of words, # strands
testManyWords :: Natural -> Natural -> Natural -> StdGen -> ([Test], StdGen)
testManyWords 0 _ _ g = ([], g)
testManyWords count wordLength strandCount gen =
  let (str, gen') = randomString wordLength strandCount gen
   in let test = testWord str
       in let (rest, gen'') = testManyWords (count - 1) wordLength strandCount gen'
           in (test : rest, gen'')

-- # of tests, length of words, # strands
testManyWordsComplete :: Natural -> Natural -> Natural -> StdGen -> ([Test], StdGen)
testManyWordsComplete 0 _ _ g = ([], g)
testManyWordsComplete count wordLength strandCount gen =
  let (word, gen') = randomProperWord wordLength strandCount gen
   in let test = testWordComplete word
       in let (rest, gen'') = testManyWordsComplete (count - 1) wordLength strandCount gen'
           in (test : rest, gen'')

testWordComplete :: BraidWord -> Test
testWordComplete w = let str = showWord w in
  TestLabel str $ TestCase (assertEqual ("Grid should be complete on " ++ str) Nothing
  (case complete . snd . reverseWord $ w of
    Err err -> Just err
    OK c -> validate c w
    ))
