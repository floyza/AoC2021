module AoC9 where

import Control.Applicative (Applicative (liftA2))
import Control.Arrow (Arrow (first))
import Data.Array (Array, bounds, listArray, (!))
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set (Set (..))
import qualified Data.Set as S

type HeightMap = Array Int (Array Int Int)

input :: IO HeightMap
input =
  do
    m <- (map . map) (read . (: [])) . lines <$> readFile "inputs/9.txt"
    return $ listArray (0, length m -1) [listArray (0, length x -1) x | x <- m]

index arr (x, y) = arr ! y ! x

within (x, y) arr =
  let (ly, hy) = bounds arr
      (lx, hx) = bounds (arr ! 0)
   in x >= lx && x <= hx && y >= ly && y <= hy

get :: Array Int (Array Int Int) -> (Int, Int) -> Maybe Int
get a pt@(x, y) = if pt `within` a then Just (a ! y ! x) else Nothing

adjacents :: HeightMap -> [((Int, Int), [Int])]
adjacents arr = concat [go arr (0, y) | y <- [fst (bounds arr) .. snd (bounds arr)]]
  where
    go arr (x, y) =
      let (_, hx) = bounds (arr ! 0)
       in if x > hx
            then []
            else ((x, y), mapMaybe (get arr) [(x -1, y), (x, y -1), (x + 1, y), (x, y + 1)]) : go arr (x + 1, y)

allBigger :: (Int, [Int]) -> Bool
allBigger (a, l) = all (> a) l

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

floodFill :: HeightMap -> (Int, Int) -> Set (Int, Int)
floodFill = go S.empty
  where
    go :: Set (Int, Int) -> HeightMap -> (Int, Int) -> Set (Int, Int)
    go set hm pt@(x, y) =
      let adj = filter ((`within` hm) <&&> (`S.notMember` set) <&&> ((/= 9) . index hm)) [(x -1, y), (x, y -1), (x + 1, y), (x, y + 1)]
       in foldl' (\s p -> go s hm p) (S.insert pt set) adj

solution1 = do
  i <- input
  let adj = adjacents i
  return $ sum $ (+ 1) . fst <$> filter allBigger (fmap (first (index i)) adj)

solution2 = do
  i <- input
  let adj = adjacents i
  let lowSpots = fst <$> filter (allBigger . first (index i)) adj
  let basins = floodFill i <$> lowSpots
  return $ product $ take 3 $ reverse $ sort $ S.size <$> basins
