import Data.List
import System.IO

diffs :: [Int] -> [Int]
diffs (x : y : xs) = abs (x - y) : diffs (y : xs)
diffs _ = []

safe :: [Int] -> Bool
safe xs = (inc xs || inc (reverse xs)) && 1 <= minDiff && maxDiff <= 3
  where
    differences = diffs xs
    maxDiff = maximum differences
    minDiff = minimum differences
    inc (x : y : xs) = x < y && inc (y : xs)
    inc _ = True

deleteAt idx xs = lft ++ rgt
  where
    (lft, _ : rgt) = splitAt idx xs

tolerantSafe :: [Int] -> Bool
tolerantSafe xs = any (safe . (`deleteAt` xs)) [0 .. length xs - 1]

parseIn :: IO [[Int]]
parseIn = do
  contents <- readFile "in.txt"
  return $ map (map read . words) (lines contents)

partOne :: IO ()
partOne = do
  reports <- parseIn
  print $ length $ filter safe reports

partTwo :: IO ()
partTwo = do
  reports <- parseIn
  print $ length $ filter tolerantSafe reports
