import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

pairs :: (Eq a) => [a] -> [(a, a)]
pairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

inBound :: (Int, Int) -> (Int, Int) -> Bool
inBound (m, n) (i, j) = 0 <= i && i < m && 0 <= j && j < n

type Roof = Seq (Seq (Int, Int, Char))

findAntennas :: Roof -> Map Char [(Int, Int)]
findAntennas Empty = Map.empty
findAntennas (xs :<| xss) = Map.unionWith (++) m (findAntennas xss)
  where
    m = foldr (\(i, j, c) m' -> Map.insertWith (++) c [(i, j)] m') Map.empty xs

findAntinodes :: Map Char [(Int, Int)] -> (Int, Int) -> [Int] -> Set (Int, Int)
findAntinodes m bound ks = Set.fromList $ concatMap (\(c, xs) -> if c == '.' then [] else forAntennaType xs) (Map.toList m)
  where
    forAntennaType xs = concat [forAntennaPair x y | (x, y) <- pairs xs]
    forAntennaPair (i, j) (i', j') =
      let di = i - i'
          dj = j - j'
       in filter (inBound bound) [(i + k * di, j + k * dj) | k <- ks]

parseIn :: IO Roof
parseIn = do
  contents <- readFile "in.txt"
  return $ Seq.fromList $ [Seq.fromList [(i, j, x) | (j, x) <- zip [0 ..] xs] | (i, xs) <- zip [0 ..] (lines contents)]

solve :: [Int] -> IO Int
solve ks = do
  roof <- parseIn
  let antennas = findAntennas roof
  let antinodes = findAntinodes antennas (Seq.length roof, Seq.length (roof `Seq.index` 0)) ks
  return $ length antinodes

partOne :: IO ()
partOne = solve [1, -2] >>= print

partTwo :: IO ()
partTwo = solve [-50 .. 50] >>= print