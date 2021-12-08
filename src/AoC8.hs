{-# LANGUAGE TupleSections #-}

module AoC8 where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (assert)
import Data.Foldable (foldl')
import Data.List (intersect, isSubsequenceOf, permutations, sort, subsequences, (\\))
import Data.Maybe (fromJust)

data Segment = A | B | C | D | E | F | G deriving (Eq, Show, Ord)

type Disp = [Segment]

-- we need a map from Segment to Segment as the result, but our knowledge is an alist of Segment to
-- multiple segments: the possibilities
-- note: missing values means all possibilities are valid, and alist must be sorted
newtype Knowledge = Knowledge [(Segment, [Segment])] deriving (Eq, Show)

instance Semigroup Knowledge where
  (Knowledge x) <> (Knowledge y) = Knowledge $ go x y
    where
      go a1@((k1, v1) : xs1) a2@((k2, v2) : xs2)
        | k1 == k2 = (k1, intersect v1 v2) : go xs1 xs2
        | k1 < k2 -- k2 is missing its value
          =
          (k1, v1) : go xs1 a2
        | k1 > k2 -- k1 is missing its value
          =
          (k2, v2) : go a1 xs2
      go [] [] = []
      go x [] = x
      go [] x = x

instance Monoid Knowledge where
  mempty = Knowledge []

-- gets the 'strong sets' out
-- a strong set is a set of outputs with length n
-- where there are n segments that have the strong set as their possibilities.
-- Example: if knowledge contains (a -> [f,g]) and (b -> [f,g]), then [f,g] is a strong set
-- any element in a strong set can be removed from everything that does not have the strong set as its possibilities
-- in the previous example, that would result in f and g being removed from the possibilities of all but a and b.
sset :: Knowledge -> [[Segment]]
sset (Knowledge know) = p
  where
    inside a@((k, v) : xs) = length a == length v && all ((== v) . snd) xs
    inside [] = False
    p = map (snd . head) $ filter inside (subsequences know)

-- remove extraneous elements of strong sets
think :: Knowledge -> Knowledge
think (Knowledge know) = Knowledge $ foldl' (flip remove) know (sset (Knowledge know))
  where
    remove r ((k, v) : xs)
      | v == r = (k, v) : remove r xs
      | otherwise = (k, v \\ r) : remove r xs
    remove _ [] = []

chrToSeg :: Char -> Segment
chrToSeg 'a' = A
chrToSeg 'b' = B
chrToSeg 'c' = C
chrToSeg 'd' = D
chrToSeg 'e' = E
chrToSeg 'f' = F
chrToSeg 'g' = G

parse :: String -> ([Disp], [Disp])
parse s =
  let (l, r) = break (== "|") (words s)
   in (sort . map chrToSeg <$> l, sort . map chrToSeg <$> tail r)

input :: IO [([Disp], [Disp])]
input = map parse . lines <$> readFile "inputs/8.txt"

disps :: [[Segment]]
disps =
  [ [A, B, C, E, F, G],
    [C, F],
    [A, C, D, E, G],
    [A, C, D, F, G],
    [B, C, D, F],
    [A, B, D, F, G],
    [A, B, D, E, F, G],
    [A, C, F],
    [A, B, C, D, E, F, G],
    [A, B, C, D, F, G]
  ]

num2Segs :: Int -> [Segment]
num2Segs = (disps !!)

complete :: Knowledge -> Knowledge
complete = (<>) fullKnowledge
  where
    segments = [A, B, C, D, E, F, G]
    fullKnowledge = Knowledge [(x, segments) | x <- segments]

deduce :: Disp -> Knowledge
deduce s = complete $ case length s of
  2 -> Knowledge $ map (,num2Segs 1) s
  4 -> Knowledge $ map (,num2Segs 4) s
  3 -> Knowledge $ map (,num2Segs 7) s
  -- 7 -> Knowledge $ map (,num2Segs 8) s -- doesn't narrow
  _ -> mempty -- does not narrow

deduction :: [Disp] -> Knowledge
deduction = mconcat . map deduce

indices :: [Bool] -> [Int]
indices = go 0
  where
    go acc (True : xs) = acc : go (acc + 1) xs
    go acc (False : xs) = go (acc + 1) xs
    go _ [] = []

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

possibleDisps :: Knowledge -> Disp -> [Int]
possibleDisps k d = result
  where
    (Knowledge l) = k
    k' = filter ((`elem` d) . fst) l -- knowledge restricted to `d`
    v = map snd k' -- values
    ss = sset (Knowledge k')
    result =
      indices $
        map
          ( (`isSubsequenceOf` sort (concat v))
              <&&> (sort (concat ss) `isSubsequenceOf`)
              <&&> ((== length d) . length)
          )
          disps

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a =
  let new = f a
   in if a == new
        then a
        else fixedPoint f new

solve2 :: ([Disp], [Disp]) -> Int
solve2 (d, out) =
  let dat = d ++ out
      possibilities = fixedPoint think $ deduction dat
   in let [[a], [b], [c], [d]] = possibleDisps possibilities <$> out
       in a * 1000 + b * 100 + c * 10 + d

solution1 :: IO Int
solution1 = do
  i <- input
  return $ length $concat $filter (\x -> let l = length x in l == 2 || l == 4 || l == 3 || l == 7) . snd <$> i

solution2 :: IO Int
solution2 = sum . map solve2 <$> input
