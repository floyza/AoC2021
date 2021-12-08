module AoC5 where

import Data.List

data Point = Point Int Int deriving (Eq, Show, Ord)

data Line = Line Point Point deriving (Eq, Show)

parse :: String -> Line
parse s = Line (Point (read a) (read b)) (Point (read c) (read d))
  where
    (a, s') = break (== ',') s
    (b, s'') = break (== ' ') (tail s')
    (c, s''') = break (== ',') (dropWhile (`elem` "-> ") s'')
    d = tail s'''

moveInt :: Int -> Int -> Int
moveInt x y = if x == y then x else (if x < y then x + 1 else x -1)

movePoint :: Point -> Point -> Point
movePoint (Point x1 y1) (Point x2 y2)
  | x1 == x2 = Point x1 (moveInt y1 y2)
  | y1 == y2 = Point (moveInt x1 x2) y2
  | abs (x1 - x2) == abs (y1 - y2) = Point (moveInt x1 x2) (moveInt y1 y2)
  | otherwise = undefined

removethis (Point x1 y1) (Point x2 y2) = (x1 /= x2) && (y1 /= y2) && (abs (x1 - x2) == abs (y1 - y2))

pointsOnLine :: Line -> [Point]
pointsOnLine (Line x y)
  | removethis x y = []
  | x == y = [x]
  | otherwise = x : pointsOnLine (Line (movePoint x y) y)

pointsOnLine' :: Line -> [Point]
pointsOnLine' (Line x y)
  | x == y = [x]
  | otherwise = x : pointsOnLine' (Line (movePoint x y) y)

allPoints :: [Line] -> [Point]
allPoints = concatMap pointsOnLine

allPoints' :: [Line] -> [Point]
allPoints' = concatMap pointsOnLine'

count :: Ord a => [a] -> [Int]
count = go 0 . sort
  where
    go acc (x : y : xs)
      | x == y = go (acc + 1) (y : xs)
      | otherwise = (acc + 1) : go 0 (y : xs)
    go acc [_] = [acc + 1]
    go acc [] = []

input :: IO [Line]
input = (map parse . lines) <$> readFile "inputs/5.txt"

solution1 = do
  i <- input
  let points = allPoints i
  return $ length $ filter (>= 2) $ count points

solution2 = do
  i <- input
  let points = allPoints' i
  return $ length $ filter (>= 2) $ count points
