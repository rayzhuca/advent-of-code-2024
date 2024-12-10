import qualified Data.Foldable as Foldable
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

swapElementsAt :: Int -> Int -> Seq a -> Seq a
swapElementsAt i j xs =
  let x = Seq.index xs i
      y = Seq.index xs j
   in Seq.update j x (Seq.update i y xs)

expand :: [Int] -> Seq Int
expand xs = Seq.fromList $ concat [replicate x (if even i then i `div` 2 else (-1)) | (i, x) <- zip [0 ..] xs]

findIndexWith :: (Int -> a -> Bool) -> Seq a -> Maybe Int
findIndexWith predicate xs =
  let indexedSeq = zip [0 ..] (Foldable.toList xs) -- Pair indices with elements
   in fst <$> Foldable.find (uncurry predicate) indexedSeq

compress :: Seq Int -> Seq Int
compress xs = case (i, j) of
  (Just i', Just j') | i' < j' -> compress $ swapElementsAt i' j' xs
  _ -> xs
  where
    i = Seq.elemIndexL (-1) xs
    j = i >>= const (Seq.findIndexR (/= (-1)) xs)

checkSum :: Seq Int -> Int
checkSum xs = sum $ zipWith (\i x -> if x /= -1 then i * x else 0) [0 ..] (Foldable.toList xs)

parseIn :: IO [Int]
parseIn = do
  contents <- readFile "in.txt"
  return $ map (read . pure :: Char -> Int) contents

partOne :: IO ()
partOne = do
  diskmap <- parseIn
  print $ checkSum $ compress $ expand diskmap

addFlag :: [Int] -> [(Bool, Int)]
addFlag xs = [(even i, x) | (i, x) <- zip [0 ..] xs]

insertAt :: Int -> a -> [a] -> [a]
insertAt z y xs = as ++ (y : bs)
  where
    (as, bs) = splitAt z xs

deleteAt idx xs = lft ++ rgt
  where
    (lft, (_ : rgt)) = splitAt idx xs

subtractNth :: Int -> Int -> [(Bool, Int)] -> [(Bool, Int)]
subtractNth i v xs = [if k == i then (f, x - v) else (f, x) | (k, (f, x)) <- zip [0 ..] xs]

splitBlock :: [(Bool, Int)] -> Int -> Int -> [(Bool, Int)]
splitBlock xs i v = subtractNth (i + 1) (snd x) xs'
  where
    x = xs !! i
    xs' = insertAt i (True, v) xs

moveBlock :: [(Bool, Int)] -> Int -> Int -> [(Bool, Int)]
moveBlock xs i j = deleteAt (j + 1) xs'
  where
    v = snd (xs !! i)
    xs' = splitBlock xs i v

moveFile :: [(Bool, Int)] -> Int -> [(Bool, Int)]
moveFile xs i =
  let (f, v) = xs !! i
      -- Find the leftmost contiguous free space that can fit the file
      freeSpaces =
        [ (j, freeLen)
          | (j, chunk) <- zip [0 ..] (tails xs),
            let freeLen = sum [x | (f', x) <- chunk, not f'],
            freeLen >= v && all (not . fst) (take (length chunk) chunk)
        ]
   in case freeSpaces of
        (j : _) ->
          if f -- Only move if it's a file (f == True)
            then
              let (before, rest) = splitAt i xs
                  (toMove, after) = splitAt 1 rest
                  (freeBefore, freeAfter) = splitAt j xs
                  newXs = freeBefore ++ toMove ++ freeAfter ++ after
               in newXs
            else xs
        _ -> xs -- No space to move the file

-- Compress the list by moving files in reverse order
compress' :: [(Bool, Int)] -> [(Bool, Int)]
-- compress' xs = moveFile xs (length xs - 1)

compress' xs = foldl moveFile xs (reverse [0 .. length xs - 1])

expand' :: Int -> [(Bool, Int)] -> [Int]
expand' _ [] = []
expand' i ((f, x) : xs) = if f then replicate x i ++ expand' (i + 1) xs else expand' i xs

partTwo :: IO ()
partTwo = do
  diskmap <- parseIn
  print $ checkSum $ Seq.fromList $ expand' 0 $ compress' $ addFlag diskmap