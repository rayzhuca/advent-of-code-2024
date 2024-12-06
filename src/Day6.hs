import Data.Bifunctor (bimap)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph = Map (Int, Int) Char

type Visited = Set (Int, Int, Char)

parseIn :: IO Graph
parseIn = do
  contents <- readFile "in.txt"
  return $ Map.fromList [((i, j), c) | (i, l) <- zip [0 ..] (lines contents), (j, c) <- zip [0 ..] l]

isPath :: Char -> Bool
isPath c = c `elem` "><^v"

dir :: Char -> (Int, Int)
dir '>' = (0, 1)
dir '<' = (0, -1)
dir '^' = (-1, 0)
dir 'v' = (1, 0)
dir _ = error "Invalid dir input"

turn :: Char -> Char
turn '>' = 'v'
turn 'v' = '<'
turn '<' = '^'
turn '^' = '>'
turn _ = error "Invalid turn input"

move :: Int -> Int -> Char -> (Int, Int)
move i j c = bimap (i +) (j +) (dir c)

step :: Graph -> Int -> Int -> Char -> (Int, Int, Char)
step m i j c = (i', j', c')
  where
    wall = case m !? move i j c of
      Just x' -> x' == '#'
      Nothing -> False
    c' = if wall then turn c else c
    (i', j') = if wall then (i, j) else move i j c'

findStart :: Graph -> ((Int, Int), Char)
findStart m = 0 `Map.elemAt` Map.filter isPath m

travel' :: Graph -> Visited -> Int -> Int -> Char -> (Graph, Bool)
travel' m v i j c =
  case m !? (i, j) of
    Just x
      | x /= c ->
          let (i', j', c') = step m i j c
              seen = Set.member (i, j, c) v
              m' = Map.insert (i, j) c m
              v' = Set.insert (i, j, c) v
           in if seen then (m', True) else travel' m' v' i' j' c'
      | otherwise -> (m, True)
    _ -> (m, False)

travel :: Graph -> (Graph, Bool)
travel m = travel' m' Set.empty i j c
  where
    ((i, j), c) = findStart m
    m' = Map.insert (i, j) '.' m

partOne :: IO ()
partOne = do
  m <- parseIn
  let (m', _) = travel m
  print $ Map.size $ Map.filter isPath m'

partTwo :: IO ()
partTwo = do
  m <- parseIn
  let candidates = Map.filter (== '.') m
  print $ Map.size $ Map.filterWithKey (\(i, j) _ -> snd $ travel $ Map.insert (i, j) '#' m) candidates
