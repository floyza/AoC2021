{-# LANGUAGE OverloadedStrings #-}

module AoC1 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

input :: IO [Int]
input = (fmap . fmap) read (lines <$> readFile "inputs/1.txt")

solve1 :: [Int] -> Int
solve1 s = go s 0
  where
    go (x : x' : xs) i = if x' > x then go (x' : xs) (i + 1) else go (x' : xs) i
    go [x] i = i
    go [] i = i

solve1' :: [Int] -> Int
solve1' =
  snd
    . foldr
      ( \x (lst, acc) ->
          if x > lst
            then (x, acc + 1)
            else (x, acc)
      )
      (1249821948, 0)
    . reverse

solution1 = solve1 <$> input

red :: [Int] -> [Int]
red (a : b : c : xs) = (a + b + c) : red (b : c : xs)
red _ = []

solve2 :: [Int] -> Int
solve2 = solve1 . red

solution2 = solve2 <$> input
