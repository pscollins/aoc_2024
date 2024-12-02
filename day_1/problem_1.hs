import System.Environment
import Data.List (sort)


main = do
  args <- getArgs
  content <- readFile (args !! 0)
  print $ diff $ both sort $ parseIn content

both :: (a -> b) -> (a, a) -> (b, b)
both f (l, r) = (f l, f r)

parseIn :: String -> ([Int], [Int])
parseIn str =
  let append :: String -> ([String], [String]) -> ([String], [String])
      append line (l, r) =
        case (words line) of
          [newL, newR] -> (newL:l, newR:r)
          _ -> error "misparse"
      toInt = both (map read)
  in
   toInt $ foldr append ([], []) $ lines str


diff :: ([Int], [Int]) -> Int
diff (l, r) =
  foldr (+) 0 $ map abs $ zipWith (-) l r
