import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq

countXmasAt :: Seq (Seq Char) -> Int -> Int -> Int
countXmasAt a i j = foldr (\l acc -> acc + (if l == map Just "XMAS" then 1 else 0)) 0 directions
  where
    walk = zipWith (\p q -> (a !? (i + p)) >>= (!? (j + q)))
    forward = [0 .. 3]
    backward = reverse [-3 .. 0]
    stay = repeat 0
    directions =
      [ walk stay forward,
        walk backward forward,
        walk backward stay,
        walk backward backward,
        walk stay backward,
        walk forward backward,
        walk forward stay,
        walk forward forward
      ]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

countXmas :: Seq (Seq Char) -> Int
countXmas a = sum $ map (uncurry $ countXmasAt a) (cartProd [0 .. n - 1] [0 .. m - 1])
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
    print $ countXmas a
