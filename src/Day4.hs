import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

walk :: Seq (Seq Char) -> Int -> Int -> [Int] -> [Int] -> [Maybe Char]
walk a i j = zipWith (\p q -> (a !? (i + p)) >>= (!? (j + q)))

countXmasAt :: Seq (Seq Char) -> Int -> Int -> Int
countXmasAt a i j = foldr (\l acc -> acc + boolToInt (l == map Just "XMAS")) 0 directions
  where
    forward = [0 .. 3]
    backward = reverse [-3 .. 0]
    stay = repeat 0
    directions =
      [ walk a i j stay forward,
        walk a i j backward forward,
        walk a i j backward stay,
        walk a i j backward backward,
        walk a i j stay backward,
        walk a i j forward backward,
        walk a i j forward stay,
        walk a i j forward forward
      ]

countXmasAt' :: Seq (Seq Char) -> Int -> Int -> Int
countXmasAt' a i j = boolToInt $ mas 1 1 && mas 1 (-1)
  where
    mas x y = let l = walk a i j [x, 0, -x] [y, 0, -y] in l == map Just "MAS" || l == map Just "SAM"

countXmas :: (Seq (Seq Char) -> Int -> Int -> Int) -> Seq (Seq Char) -> Int
countXmas f a = sum $ map (uncurry $ f a) (cartProd [0 .. n - 1] [0 .. m - 1])
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
    print $ countXmas countXmasAt a

partTwo :: IO ()
partTwo =
  do
    a <- parseIn
    print $ countXmas countXmasAt' a
