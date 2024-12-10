import Control.Monad.State
import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

at :: Seq (Seq Char) -> Int -> Int -> Maybe Char
at a i j = a !? i >>= (!? j)

dfs :: Seq (Seq Char) -> Int -> Int -> Set (Int, Int) -> State (Set (Int, Int)) ()
dfs grid i j visited = do
  v <- get
  case at grid i j of
    Nothing -> return ()
    Just currentVal ->
      if Set.member (i, j) v
        then return ()
        else do
          put (Set.insert (i, j) v)

          let neighbors = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
          let validNeighbors =
                filter
                  ( \(ni, nj) -> case at grid ni nj of
                      Just neighborVal -> neighborVal == succ currentVal
                      Nothing -> False
                  )
                  neighbors
          mapM_ (\(ni, nj) -> dfs grid ni nj visited) validNeighbors

dfs' :: Seq (Seq Char) -> Int -> Int -> State Int ()
dfs' grid i j = do
  v <- get
  case at grid i j of
    Nothing -> return ()
    Just currentVal ->
      do
        if currentVal == '9'
          then
            put (v + 1)
          else do
            let neighbors = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
            let validNeighbors =
                  filter
                    ( \(ni, nj) -> case at grid ni nj of
                        Just neighborVal -> neighborVal == succ currentVal
                        Nothing -> False
                    )
                    neighbors
            mapM_ (uncurry $ dfs' grid) validNeighbors

score :: Seq (Seq Char) -> Int -> Int -> Int
score a i j = Set.size $ Set.filter (\(i', j') -> at a i' j' == Just '9') visited
  where
    visited = execState (dfs a i j Set.empty) Set.empty

score' :: Seq (Seq Char) -> Int -> Int -> Int
score' a i j = execState (dfs' a i j) 0

totalScore :: (Seq (Seq Char) -> Int -> Int -> Int) -> Seq (Seq Char) -> Int
totalScore f a = sum $ map (\(i, j) -> if at a i j == Just '0' then f a i j else 0) (cartProd [0 .. n - 1] [0 .. m - 1])
  where
    n = length a
    m = length (Seq.index a 0)

parseIn :: IO (Seq (Seq Char))
parseIn = do
  contents <- readFile "in.txt"
  let contents' = lines contents
  return $ Seq.fromList <$> Seq.fromList contents'

partOne :: IO ()
partOne =
  do
    a <- parseIn
    print $ totalScore score a

partTwo :: IO ()
partTwo =
  do
    a <- parseIn
    print $ totalScore score' a
