module AoC2 where

data Command = Command Dir Int deriving (Eq, Show)

data Dir = Forward | Down | Up deriving (Eq, Show)

readCommand ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : xs) = Command Forward (read xs)
readCommand ('u' : 'p' : ' ' : xs) = Command Up (read xs)
readCommand ('d' : 'o' : 'w' : 'n' : ' ' : xs) = Command Down (read xs)

input :: IO [Command]
input = (fmap . fmap) readCommand (lines <$> readFile "inputs/2.txt")

data Position = Pos Int Int deriving (Eq, Show)

follow :: Position -> [Command] -> Position
follow (Pos d p) ((Command Forward i) : xs) = follow (Pos d (p + i)) xs
follow (Pos d p) ((Command Up i) : xs) = follow (Pos (d - i) p) xs
follow (Pos d p) ((Command Down i) : xs) = follow (Pos (d + i) p) xs
follow pos [] = pos

solution1 = do
  i <- input
  return $ follow (Pos 0 0) i

data Position2 = Pos2 Int Int Int deriving (Eq, Show)

follow2 :: Position2 -> [Command] -> Position2
follow2 (Pos2 d p a) ((Command Forward i) : xs) = follow2 (Pos2 (d + a * i) (p + i) a) xs
follow2 (Pos2 d p a) ((Command Up i) : xs) = follow2 (Pos2 d p (a - i)) xs
follow2 (Pos2 d p a) ((Command Down i) : xs) = follow2 (Pos2 d p (a + i)) xs
follow2 pos [] = pos

solution2 = do
  i <- input
  return $ follow2 (Pos2 0 0 0) i
