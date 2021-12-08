module AoC7 where

import Data.List
import Data.Ord

parse :: String -> [Int]
parse "" = []
parse s = read a : parse (tail s')
  where
    (a, s') = break (\x -> x == ',' || x == '\n') s

-- parse s = Line (Point (read a) (read b)) (Point (read c) (read d))
--   where
--     (a, s') = break (== ',') s
--     (b, s'') = break (== ' ') (tail s')
--     (c, s''') = break (== ',') (dropWhile (`elem` "-> ") s'')
--     d = tail s'''

-- parse :: String -> [Int]
-- parse (',' : xs) = parse xs
-- parse ('\n' : xs) = parse xs
-- parse (x : xs) = read [x] : parse xs
-- parse [] = []

input :: IO [Int]
input = parse <$> readFile "inputs/7.txt"

distance :: Int -> Int -> Int
distance x y = abs (x - y)

maneuver :: [Int] -> (Int, Int)
maneuver crabs = minimumBy (comparing snd) [(p, sum $ map (distance p) crabs) | p <- [min .. max]]
  where
    min = minimum crabs
    max = maximum crabs

inc x
  | odd x = x * ((x `div` 2) + 1)
  | otherwise = (x + 1) * (x `div` 2)

maneuver2 :: [Int] -> (Int, Int)
maneuver2 crabs = minimumBy (comparing snd) [(p, sum $ map (inc . distance p) crabs) | p <- [min .. max]]
  where
    min = minimum crabs
    max = maximum crabs
