{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AoC12 where

import Control.Monad.Trans.State
import Data.Char
import Data.Map ((!))
import qualified Data.Map.Strict as M
import Data.MemoTrie
import GHC.Generics

data Connection a = Connection a a deriving (Eq, Show, Generic)

instance HasTrie a => HasTrie (Connection a) where
  newtype Connection a :->: b = ConnectionTrie {unConnectionTrie :: Reg (Connection a) :->: b}
  trie = trieGeneric ConnectionTrie
  untrie = untrieGeneric unConnectionTrie
  enumerate = enumerateGeneric unConnectionTrie

newtype Graph a = Graph [Connection a] deriving (Eq, Show, Generic)

instance HasTrie a => HasTrie (Graph a) where
  newtype Graph a :->: b = GraphTrie {unGraphTrie :: Reg (Graph a) :->: b}
  trie = trieGeneric GraphTrie
  untrie = untrieGeneric unGraphTrie
  enumerate = enumerateGeneric unGraphTrie

instance (Ord a, HasTrie a, HasTrie b) => HasTrie (M.Map a b) where
  newtype M.Map a b :->: c = MapTrie {unMapTrie :: [(a, b)] :->: c}
  trie f = MapTrie $ trie $ f . M.fromList
  untrie (MapTrie m) = untrie m . M.toList

data TravellingGraph a = TravellingGraph (Graph a) (M.Map a Int) deriving (Eq, Show, Generic)

instance (Ord a, HasTrie a) => HasTrie (TravellingGraph a) where
  newtype TravellingGraph a :->: b = TravellingGraphTrie {unTravellingGraphTrie :: Reg (TravellingGraph a) :->: b}
  trie = trieGeneric TravellingGraphTrie
  untrie = untrieGeneric unTravellingGraphTrie
  enumerate = enumerateGeneric unTravellingGraphTrie

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

data Cave = Big | Small deriving (Eq, Show, Ord, Generic)

instance HasTrie Cave where
  newtype Cave :->: a = CaveTrie {unCaveTrie :: Reg Cave :->: a}
  trie = trieGeneric CaveTrie
  untrie = untrieGeneric unCaveTrie
  enumerate = enumerateGeneric unCaveTrie

data Pt = Pt Int Cave deriving (Eq, Show, Ord, Generic)

instance HasTrie Pt where
  newtype Pt :->: a = PtTrie {unPtTrie :: Reg Pt :->: a}
  trie = trieGeneric PtTrie
  untrie = untrieGeneric unPtTrie
  enumerate = enumerateGeneric unPtTrie

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

travel :: Int -> TravellingGraph Pt -> Int
travel travelCount graph = go startPt graph
  where
    go :: Pt -> TravellingGraph Pt -> Int
    go = memo2 travelStep
    travelStep :: Pt -> TravellingGraph Pt -> Int
    travelStep x (TravellingGraph graph mp) =
      if x == endPt
        then 1
        else
          let paths = filter (\p@(Pt i e) -> isBig e || ((mp ! p) < travelCount)) $ nextSteps graph x
           in sum $ (\n -> go n (TravellingGraph graph (M.adjust (+ 1) n mp))) <$> paths

solution1 = travel 1 <$> input

solution2 = travel 2 <$> input
