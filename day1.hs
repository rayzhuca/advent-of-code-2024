import System.IO
import Data.List

totalDist :: [Int] -> [Int] -> Int
totalDist a b = sum (zipWith (curry (abs . uncurry (-))) a b)

score :: [Int] -> [Int] -> Int
score a b = sum (map (\x -> x * length (filter (==x) b)) a)

split :: [Int] -> ([Int], [Int])
split [] = ([], [])
split (a:b:xs) = (a : as, b : bs)
  where (as, bs) = split xs

parseIn :: IO ([Int], [Int])
parseIn = do
    content <- readFile "in.txt"
    let numbers = map read (words content) :: [Int]
    return (split numbers)

partOne :: IO ()
partOne = do
    (a, b) <- parseIn
    let a' = sort a
    let b' = sort b
    let result = totalDist a' b'
    print result

partTwo :: IO ()
partTwo = do
    (a, b) <- parseIn
    print (score a b)

