module AoC6 where

import Data.Function (fix)
import Data.MemoTrie (memo)

type School = [Int]

parse :: String -> School
parse (',' : xs) = parse xs
parse ('\n' : xs) = parse xs
parse (x : xs) = read [x] : parse xs
parse [] = []

grow :: School -> School
grow (0 : xs) = 8 : 6 : grow xs
grow (x : xs) = (x -1) : grow xs
grow [] = []

sizeAfter :: Int -> School -> Int
sizeAfter i s = length $ iterate grow s !! i

input :: IO School
input = parse <$> readFile "inputs/6.txt"

result1 :: IO Int
result1 = sizeAfter 80 <$> input

offSpring :: Int -> Int -> Int
offSpring days x = f (days - x)

-- the amount of fish that will exist after `x` days,
-- starting with one fish that will have offspring on this day
f :: Int -> Int
f = memo go
  where
    go days
      | days <= 0 = 1
      | otherwise = f daysLeft + f (daysLeft -2)
      where
        daysLeft = days - 7

sizeAfter' :: Int -> School -> Int
sizeAfter' i s = sum $ offSpring i <$> s

result2 :: IO Int
result2 = do
  sizeAfter' 256 <$> input
