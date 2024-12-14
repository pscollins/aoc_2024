{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Debug.Trace

import Test.Hspec
import Test.Hspec.Core.Runner


testAll :: Spec
testAll = do
  describe "trivial" $ do
    it "basic" $ do
      1 `shouldBe` 1

evalTests :: IO ()
evalTests = do
  (config, forest) <- evalSpec defaultConfig testAll
  runSpecForest forest config
  print "tests finished"

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  evalTests
  print $ content
