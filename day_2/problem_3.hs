{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Data.List (sort)
import Debug.Trace

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  print $ count $ isValid $ diff $ parseIn content


parseIn :: String -> [[Int]]
parseIn = map (map read . words) . lines


diff :: [[Int]] -> [[Int]]
diff valuess =
  let
    diff (l, r) = r - l
    getDiff values =
      map diff $ zip values $ tail values
  in
    map getDiff valuess


isValid :: [[Int]] -> [Bool]
isValid diffss =
  let
    decreasing = all (< 0)
    increasing = all (> 0)
    bounded = all (< 4) . map abs
    validLine diffs =
      ((decreasing diffs) || (increasing diffs)) &&
      bounded diffs
  in
    map validLine diffss


count :: [Bool] -> Int
count = length . filter (==True)
