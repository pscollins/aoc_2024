{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Data.List (sort)
import Debug.Trace

import Test.Hspec
import Test.Hspec.Core.Runner


testAll :: Spec
testAll = do
  describe "simple" $ do
    it "simple" $ do
      1 `shouldBe` 1

evalTests :: IO ()
evalTests = do
  (config, forest) <- evalSpec defaultConfig testAll
  runSpecForest forest config
  print "tests finished"
  -- print summary

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  evalTests
  print "hello world"


-- parseIn :: String -> [[Int]]
-- parseIn = map (map read . words) . lines


-- diff :: [[Int]] -> [[Int]]
-- diff valuess =
--   let
--     diff (l, r) = r - l
--     getDiff values =
--       map diff $ zip values $ tail values
--   in
--     map getDiff valuess

-- subLine :: [a] -> [[a]]
-- subLine xs =
--   let
--     except index =
--       [x | (i, x) <- zip [0..] xs, i /= index]
--   in
--     map except [0..length(xs)]

-- isValid :: [[Int]] -> [Bool]
-- isValid diffss =
--   let
--     decreasing = all (< 0)
--     increasing = all (> 0)
--     bounded = all (< 4) . map abs
--     validLine diffs =
--       ((decreasing diffs) || (increasing diffs)) &&
--       bounded diffs
--     anyValidLine :: [Int] -> Bool
--     anyValidLine diffs =
--       any validLine $ traceShowId $ subLine diffs
--   in
--     traceShowId $ map anyValidLine diffss

-- anyDiffValid :: [Int] -> Bool
-- anyDiffValid row =
--   let
--     diffValid subrow = isValid

-- count :: [Bool] -> Int
-- count = length . filter (==True)
