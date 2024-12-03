import Data.Char
import Data.List
import Data.Maybe
import System.IO

splitAtFirst :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

mulOperands :: String -> Maybe (Int, Int)
mulOperands s
  | fname == "mul" && all isDigit a && all isDigit b = Just (read a, read b)
  | otherwise = Nothing
  where
    (fname, rest) = splitAtFirst '(' s
    (a, rest') = splitAtFirst ',' rest
    (b, _) = splitAtFirst ')' rest'

findMuls :: String -> [(Int, Int)]
findMuls s = mapMaybe (\i -> mulOperands $ drop i s) [0 .. length s - 1]

subStrings :: String -> String -> [Int]
subStrings f x =
  subStrings' f x 0
  where
    l = length f
    subStrings' _ [] _ = []
    subStrings' f x i
      | f `isPrefixOf` x = i : subStrings' f (drop l x) (i + l)
      | otherwise = subStrings' f (drop 1 x) (i + 1)

findMuls' :: String -> [(Int, Int)]
findMuls' s = mapMaybe (\i -> mulOperands (drop i s) >>= (\x -> if enabled i then Just x else Nothing)) [0 .. length s - 1]
  where
    dos = -1 : subStrings "do()" s
    donts = -2 : subStrings "don't()" s
    enabled i = last (filter (<= i) dos) >= last (filter (<= i) donts)

total :: [(Int, Int)] -> Int
total = sum . map (uncurry (*))

partOne :: IO ()
partOne = do
  contents <- readFile "in.txt"
  let muls = findMuls contents
  print (total muls :: Int)

partTwo :: IO ()
partTwo = do
  contents <- readFile "in.txt"
  let muls = findMuls' contents
  print (total muls :: Int)
