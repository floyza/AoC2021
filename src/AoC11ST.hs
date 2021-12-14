module AoC11 where

import Control.Monad.ST
import Data.Array.ST

type Octopuses s = STArray s (Int, Int) Int

surroundings :: (Int, Int) -> [(Int, Int)]
surroundings (x, y) = [(a, b) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1]]

existingSurroundings :: Octopuses s -> (Int, Int) -> ST s [(Int, Int)]
existingSurroundings o i = do
  bound <- getBounds o
  return $ filter (inRange bound) (surroundings i)

flash :: Octopuses s -> (Int, Int) -> ST s (Octopuses s)
flash i o = undefined

advance :: Octopuses s -> ST s (Octopuses s)
advance o = do
  bound <- getBounds o
  mapArray (+ 1) o >>= mapIndices bound undefined
