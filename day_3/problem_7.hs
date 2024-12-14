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

  describe "extractSyms" $ do
    it "extracts only valid" $ do
      extractSyms "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)" `shouldBe` [
        "mul(2,4)", "mul(5,5)"
        ]

    it "extracts do" $ do
      extractSyms "xmul(2,4)%&mul[3,7]!do()@^do_not_mul(5,5)" `shouldBe` [
        "mul(2,4)", "do()", "mul(5,5)"
        ]

    it "extracts don't" $ do
      extractSyms "xmul(2,4)%&mul[3,7]!don't()@^do_not_mul(5,5)" `shouldBe` [
        "mul(2,4)", "don't()", "mul(5,5)"
        ]
    it "runs full part1" $ do
      extractSyms "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" `shouldBe` [
        "mul(2,4)", "mul(5,5)", "mul(11,8)", "mul(8,5)"
        ]

    it "runs full part2" $ do
      extractSyms "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5)" `shouldBe` [
        "mul(2,4)", "don't()", "mul(5,5)", "mul(11,8)", "do()", "mul(8,5)"
        ]

  describe "filterSyms" $ do
    it "runs full part2" $ do
      filterSyms [
        "mul(2,4)",
        "don't()",
        "mul(5,5)",
        "mul(11,8)",
        "do()",
        "mul(8,5)"
        ] `shouldBe` [
        "mul(2,4)",
        "mul(8,5)"
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
  print $ sum $ map parseMul $ filterSyms $ extractSyms content


extractSyms :: String -> [String]
extractSyms = getAllTextMatches . (=~ "mul\\(-?[0-9]+,-?[0-9]+\\)|do\\(\\)|don't\\(\\)")


filterSyms :: [String] -> [String]
filterSyms ss =
  let
    -- update :: String -> (Bool, [String]) -> (Bool, [String])
    update :: (Bool, [String]) -> String -> (Bool, [String])
    update (state, kept) s =
      case (s, state) of
        ("do()", _) -> (True, kept)
        ("don't()", _) -> (False, kept)
        (mul, True) -> (True, mul:kept)
        (mul, False) -> (False, kept)
    (_, result) = foldl update (True, []) ss
  in
    reverse result

parseMul :: String -> Int
parseMul s =
  let
    matches :: [String] = getAllTextSubmatches $ s =~ "mul\\((-?[0-9]+),(-?[0-9]+)\\)"
    parsedMatches :: [Int] = map read $ drop 1 $ matches
  in
    foldl (*) 1 parsedMatches
