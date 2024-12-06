import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

type Rules = Set (Int, Int)

type Update = (Seq Int)

toPair :: [Int] -> (Int, Int)
toPair [a, b] = (a, b)
toPair _ = error "toPair called on list on a wrong size"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

mid :: Seq Int -> Int
mid xs = let m = Seq.length xs `div` 2 in xs `Seq.index` m

parseIn :: IO (Rules, Seq Update)
parseIn =
  do
    contents <- readFile "in.txt"
    let contents' = lines contents
    let rules = Set.fromList [toPair $ map read (wordsWhen (== '|') s) | s <- contents', '|' `elem` s]
    let updates = Seq.fromList [Seq.fromList (map read (wordsWhen (== ',') s)) | s <- contents', ',' `elem` s]
    return (rules, updates)

validUpdate :: Rules -> Update -> Bool
validUpdate rules xs = all (\(x, y) -> Set.member (x, y) rules) [(xs `Seq.index` i, xs `Seq.index` j) | i <- [0 .. n - 1], j <- [0 .. n - 1], i < j]
  where
    n = Seq.length xs

partOne :: IO ()
partOne =
  do
    (rules, updates) <- parseIn
    let valids = Seq.filter (validUpdate rules) updates
    print $ sum $ fmap mid valids

correct :: Rules -> Update -> Update
correct rules xs = Seq.sortOn (\x -> sum (fmap (\y -> if Set.member (y, x) rules then 1 :: Int else 0 :: Int) xs)) xs

partTwo :: IO ()
partTwo =
  do
    (rules, updates) <- parseIn
    let invalids = Seq.filter (not . validUpdate rules) updates

    let fixes = fmap (correct rules) invalids
    print $ sum $ fmap mid fixes
