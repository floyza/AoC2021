module AoC3 where

import Control.Arrow ()
import Data.List (transpose)

input :: IO [String]
input = (fmap . fmap) id (lines <$> readFile "inputs/3.txt")

dom = dom' (0, 0)

dom' :: (Int, Int) -> [Char] -> Bool
dom' (t, f) ('1' : xs) = dom' (t + 1, f) xs
dom' (t, f) ('0' : xs) = dom' (t, f + 1) xs
dom' (t, f) [] = if t > f then True else False

gamma :: [[Char]] -> [Bool]
gamma l =
  let rows = transpose l
   in fmap dom rows

epsilon :: [[Char]] -> [Bool]
epsilon = fmap not <$> gamma

fromBin :: [Bool] -> Int
fromBin = fromBin' 0 . reverse

fromBin' :: Int -> [Bool] -> Int
fromBin' i (True : xs) = 2 ^ i + fromBin' (i + 1) xs
fromBin' i (False : xs) = fromBin' (i + 1) xs
fromBin' i [] = 0

testin = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

result1 :: IO Int
result1 = do
  x <- input
  return (fromBin (epsilon x) * fromBin (gamma x))

input2 :: IO [[Bool]]
input2 = (fmap . fmap . fmap) (\x -> if x == '1' then True else False) (lines <$> readFile "inputs/3.txt")

dom2 :: [Bool] -> Bool
dom2 = dom2' (0, 0)

dom2' :: (Int, Int) -> [Bool] -> Bool
dom2' (t, f) (True : xs) = dom2' (t + 1, f) xs
dom2' (t, f) (False : xs) = dom2' (t, f + 1) xs
dom2' (t, f) [] = if t >= f then True else False

oxygen :: [[Bool]] -> [Bool]
oxygen = solve2 0 dom2

co2 :: [[Bool]] -> [Bool]
co2 = solve2 0 (not . dom2)

solve2 :: Int -> ([Bool] -> Bool) -> [[Bool]] -> [Bool]
solve2 _ _ [l] = l
solve2 i f l =
  let rows = transpose l
      mc = f (rows !! i)
   in solve2 (i + 1) f $ filter (\x -> (x !! i) == mc) l

result2 :: IO Int
result2 = do
  x <- input2
  return (fromBin (oxygen x) * fromBin (co2 x))
