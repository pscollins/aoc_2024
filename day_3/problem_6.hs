{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Debug.Trace
import Text.Regex.TDFA

import Test.Hspec
import Test.Hspec.Core.Runner


testAll :: Spec
testAll = do
  describe "trivial" $ do
    it "basic" $ do
      1 `shouldBe` 1

  describe "parseMul" $ do
    it "supportsValid" $ do
      parseMul "mul(2,4)" `shouldBe` 8
      parseMul "mul(1,4)" `shouldBe` 4
      parseMul "mul(-1,4)" `shouldBe` -4

  describe "extractMul" $ do
    it "extracts only valid" $ do
      extractMul "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)" `shouldBe` [
        "mul(2,4)", "mul(5,5)"
        ]
    it "runs full example" $ do
      extractMul "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" `shouldBe` [
        "mul(2,4)", "mul(5,5)", "mul(11,8)", "mul(8,5)"
        ]


evalTests :: IO ()
evalTests = do
  (config, forest) <- evalSpec defaultConfig testAll
  runSpecForest forest config
  print "tests finished"

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  evalTests
  print $ sum $ map parseMul $ extractMul content


extractMul :: String -> [String]
extractMul = getAllTextMatches . (=~ "mul\\(-?[0-9]+,-?[0-9]+\\)")

parseMul :: String -> Int
parseMul s =
  let
    matches :: [String] = getAllTextSubmatches $ s =~ "mul\\((-?[0-9]+),(-?[0-9]+)\\)"
    parsedMatches :: [Int] = map read $ drop 1 $ matches
  in
    foldl (*) 1 parsedMatches
