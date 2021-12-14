{-# LANGUAGE TupleSections #-}

module AoC12 where

import Control.Monad.Trans.State
import Data.Char
import Data.Map ((!))
import qualified Data.Map.Strict as M

data Connection a = Connection a a deriving (Eq, Show)

newtype Graph a = Graph [Connection a] deriving (Eq, Show)

data TravellingGraph a = TravellingGraph (Graph a) (M.Map a Int) deriving (Eq, Show)

makeBigraph :: [(a, a)] -> Graph a
makeBigraph l = Graph $ concatMap connections l
  where
    connections (a, b) = [Connection a b, Connection b a]

splitFromElem :: Eq a => a -> [a] -> ([a], [a])
splitFromElem e (x : xs)
  | e == x = ([], xs)
  | otherwise = case splitFromElem e xs of
    (f, s) -> (x : f, s)
splitFromElem _ [] = ([], [])

parseLine :: String -> [String]
parseLine = (\(x, y) -> [x, y]) . splitFromElem '-'

data Cave = Big | Small deriving (Eq, Show, Ord)

data Pt = Pt Int Cave deriving (Eq, Show, Ord)

input :: IO (TravellingGraph Pt)
input = do
  i <- fmap parseLine . lines <$> readFile "inputs/12.txt"
  let (dat, (m, _)) = runState ((traverse . traverse) serialize i) (M.empty, 0)
  let graph = makeBigraph $ (\[x, y] -> (x, y)) <$> dat
  return $ TravellingGraph graph (M.fromList $ (startPt, 1) : ((,0) <$> (endPt : M.elems m)))

startPt = Pt (-1) Small

endPt = Pt (-2) Small

serialize :: String -> State (M.Map String Pt, Int) Pt
serialize "start" = return startPt
serialize "end" = return endPt
serialize s = do
  let cave = if isUpper (head s) then Big else Small
  (m, t) <- get
  case M.lookup s m of
    (Just x) -> return x
    Nothing ->
      let m' = M.insert s (Pt t cave) m
       in put (m', t + 1) >> return (Pt t cave)

isBig Big = True
isBig Small = False

isSmall = not . isBig

ifdo :: Bool -> (a -> a) -> a -> a
ifdo True f a = f a
ifdo False _ a = a

-- occurences :: (Num p, Eq a) => a -> [a] -> p
-- occurences e l = go 0 l
--   where
--     go i (x : xs) = go (ifdo (x == e) (+ 1) i) xs
--     go i [] = i

pathsFrom :: Eq a => Graph a -> a -> [Connection a]
pathsFrom (Graph l) x = filter pred l
  where
    pred (Connection s _) = s == x

nextSteps g x = fmap extract $ pathsFrom g x
  where
    extract (Connection a b) = b

travel :: Int -> TravellingGraph Pt -> [[Pt]]
travel travelCount graph = go [startPt] graph
  where
    go :: [Pt] -> TravellingGraph Pt -> [[Pt]]
    go currPath@(x : _) (TravellingGraph graph mp) =
      if x == endPt
        then [currPath]
        else
          let paths = filter (\p@(Pt i e) -> isBig e || ((mp ! p) < travelCount)) $ nextSteps graph x
           in concat $ (\n -> go (n : currPath) (TravellingGraph graph (M.adjust (+ 1) n mp))) <$> paths
    go [] _ = undefined

solution1 = length . travel 1 <$> input

solution2 = length . travel 2 <$> input
