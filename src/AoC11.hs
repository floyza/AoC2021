module AoC11 where

import Control.Arrow
import Control.Monad.Trans.Writer
import Data.Array
import Data.Foldable (foldrM)
import Data.Function
import Data.Maybe
import Data.Monoid

type Mat = Array (Int, Int)

type Acc = Writer (Sum Int)

surroundings :: (Int, Int) -> [(Int, Int)]
surroundings (x, y) = [(a, b) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1]]

existsIn :: Ix a => a -> Array a e -> Bool
existsIn i o = inRange (bounds o) i

flash :: (Int, Int) -> Mat (Maybe Int) -> Mat (Maybe Int)
flash i o =
  o
    // ((id &&& fmap (+ 1) . (o !)) <$> (filter (`existsIn` o) (surroundings i)))
    // [(i, Nothing)]

flashes :: Mat (Maybe Int) -> Mat (Maybe Int)
flashes o = filter ((maybe False (> 9)) . snd) (assocs o) & map fst & foldr flash o

-- untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
-- untilM p f a
--   | p a = return a
--   | otherwise = f a >>= untilM p f

advance :: Mat Int -> Acc (Mat Int)
advance o = do
  let x = fmap (+ 1) o & fmap Just & until (not . any (maybe False (> 9))) flashes
  tell $ Sum $ length $ filter (== Nothing) (elems x)
  return $ fmap (fromMaybe 0) x

listMatToMat :: [[a]] -> Array (Int, Int) a
listMatToMat l =
  let xbound = length (head l) -1
      ybound = length l -1
   in listArray ((0, 0), (xbound, ybound)) (concat l)

input :: IO (Mat Int)
input = fmap (read . (: [])) . listMatToMat . lines <$> readFile "inputs/11.txt"

applyM :: (Monad m, Eq i, Num i) => i -> (a -> m a) -> a -> m a
applyM 1 f x = f x
applyM i f x = f x >>= applyM (i -1) f

advance2 :: Mat Int -> Maybe (Mat Int)
advance2 o = do
  let x = fmap (+ 1) o & fmap Just & until (not . any (maybe False (> 9))) flashes
  if all (== Nothing) (elems x)
    then Nothing
    else Just $ fmap (fromMaybe 0) x

iterateM :: (Traversable m, Monad m) => (a -> m a) -> a -> [m a]
iterateM f a =
  let res = f a
   in res : sequence (res >>= (sequence . iterateM f))

count :: (a -> Maybe a) -> a -> Int
count f a = case f a of
  Just x -> count f x + 1
  Nothing -> 1

solution1 = execWriter . applyM 100 advance <$> input

solution2 = count advance2 <$> input
