-- Part one is just removing "read (show c ++ show x)" so it's omitted this time
valid :: (Int, [Int]) -> Int -> Bool
valid (t, x : xs) c = any (valid (t, xs)) [c + x, c * x, read (show c ++ show x)]
valid (t, []) c = t == c

parseIn :: IO [(Int, [Int])]
parseIn = do
  contents <- readFile "in.txt"
  let contents' = lines contents
  return $ map parseLine contents'
  where
    parseLine l =
      let (a, b) = break (== ':') l
       in (read a, map read $ words (tail b))

partTwo :: IO ()
partTwo =
  do
    eqs <- parseIn
    print $ sum $ map fst (filter (\(t, xs) -> valid (t, tail xs) (head xs)) eqs)
