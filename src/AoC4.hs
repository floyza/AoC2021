module AoC4 where

import Control.Arrow
import Data.List
import Data.Maybe

data Board = Board [[Int]] deriving (Show)

data SolvingBoard = SolvingBoard [[Maybe Int]] deriving (Show)

parseBoard :: [String] -> Board
parseBoard s = Board $ (fmap read . words) <$> s

splitBoards :: [String] -> [[String]]
splitBoards [] = []
splitBoards ("" : xs) = splitBoards xs
splitBoards s = take 5 s : splitBoards (drop 5 s)

parse :: String -> ([Int], [Board])
parse s =
  let l = lines s
      bin = read <$> words (map (\x -> if x == ',' then ' ' else x) (head l))
      rest = drop 2 l
   in (bin, parseBoard <$> splitBoards rest)

input :: IO ([Int], [Board])
input = parse <$> readFile "inputs/4.txt"

makeSolving :: Board -> SolvingBoard
makeSolving (Board b) = SolvingBoard $ (fmap . fmap) Just b

diag1 :: [[a]] -> [a]
diag1 lst = [lst !! i !! i | i <- [0 .. length lst - 1]]

diag2 :: [[a]] -> [a]
diag2 lst = [lst !! i !! (length lst - 1 - i) | i <- [0 .. length lst - 1]]

-- all (== Nothing) (diag1 brd) || all (== Nothing) (diag2 brd)
solved :: [[Maybe Int]] -> Bool
solved brd = any (all (== Nothing)) brd || any (all (== Nothing)) (transpose brd)

removeMatching :: Int -> [[Maybe Int]] -> [[Maybe Int]]
removeMatching x b =
  (fmap . fmap)
    ( \a -> case a of
        (Just y) -> if x == y then Nothing else Just y
        Nothing -> Nothing
    )
    b

solveBoard = solveBoard' 0

solveBoard' :: Int -> [Int] -> SolvingBoard -> Maybe (SolvingBoard, Int, Int)
solveBoard' _ [] _ = Nothing
solveBoard' i (x : xs) (SolvingBoard b) =
  let next = removeMatching x b
   in if solved next then Just (SolvingBoard next, x, i + 1) else solveBoard' (i + 1) xs (SolvingBoard next)

turn (SolvingBoard brd) = length $ filter (== Nothing) $ concat brd

score (SolvingBoard brd) = sum $ catMaybes (concat brd)

solution1 = do
  (i, brds) <- input
  let solutions = mapMaybe (solveBoard i . makeSolving) brds
  let n = sortOn thrd' solutions
  let solution = head n
  return $ uncurry (*) . threeTupToTwo $ first' score solution

threeTupToTwo (x, y, _) = (x, y)

first' f (x, y, z) = (f x, y, z)

fst' (x, _, _) = x

snd' (_, x, _) = x

thrd' (_, _, x) = x

solution2 = do
  (i, brds) <- input
  let solutions = mapMaybe (solveBoard i . makeSolving) brds
  let n = sortOn thrd' solutions
  let solution = last n
  return $ uncurry (*) . threeTupToTwo $ first' score solution
