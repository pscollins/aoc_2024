{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Debug.Trace

import Test.Hspec
import Test.Hspec.Core.Runner


testAll :: Spec
testAll = do
  describe "subLine" $ do
    it "basic" $ do
      subLine [0..2] `shouldBe` [[1, 2], [0, 2], [0, 1], [0, 1, 2]]

  describe "diff" $ do
    it "basic" $ do
      diff [2, 4, 6, 0] `shouldBe` [2, 2, -6]

  describe "parseIn" $ do
    it "basic" $ do
      parseIn "1 2 3\n4 5" `shouldBe` [[1, 2, 3], [4, 5]]

  describe "isValid" $ do
    it "supports increasing" $ do
      isValid [1, 2, 3] `shouldBe` True

    it "supports decreasing" $ do
      isValid [-1, -2, -3] `shouldBe` True

    it "rejects equal" $ do
      isValid [0] `shouldBe` False

    it "rejects large neg" $ do
      isValid [-4] `shouldBe` False

    it "rejects large pos" $ do
      isValid [4] `shouldBe` False

  describe "hasValidSubline" $ do
    it "accepts safe 1" $ do
      hasValidSubline [7, 6, 4, 2, 1] `shouldBe` True
    it "accepts safe 2" $ do
      hasValidSubline [1, 3, 6, 7, 9] `shouldBe` True

    it "recognizes single deletion" $ do
      hasValidSubline [1, 3, 2, 4, 5] `shouldBe` True
      hasValidSubline [8, 6, 4, 4, 1] `shouldBe` True

    it "rejects unsafe" $ do
      hasValidSubline [1, 2, 7, 8, 9] `shouldBe` False
      hasValidSubline [9, 7, 6, 2, 1] `shouldBe` False



evalTests :: IO ()
evalTests = do
  (config, forest) <- evalSpec defaultConfig testAll
  runSpecForest forest config
  print "tests finished"

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  evalTests
  print $ length . filter hasValidSubline $ parseIn content


parseIn :: String -> [[Int]]
parseIn = map (map read . words) . lines

diff :: [Int] -> [Int]
diff values =
  let
    diff (l, r) = r - l
  in
    map diff $ zip values $ (drop 1) values

subLine :: [a] -> [[a]]
subLine xs =
  let
    except index =
      [x | (i, x) <- zip [0..] xs, i /= index]
  in
    map except [0..length(xs)]

isValid :: [Int] -> Bool
isValid diffs =
  let
    decreasing = all (< 0)
    increasing = all (> 0)
    bounded = all (< 4) . map abs
  in
    ((decreasing diffs) || (increasing diffs)) &&
    (bounded diffs)


hasValidSubline :: [Int] -> Bool
hasValidSubline xs =
  any (isValid . diff) $ subLine xs
