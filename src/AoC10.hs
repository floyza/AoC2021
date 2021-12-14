module AoC10 where

import Control.Applicative (liftA2)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Type = Angle | Curly | Square | Curved deriving (Eq, Show)

data List = List Type [List] deriving (Eq, Show)

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

input :: IO [Text]
input = fmap T.pack . lines <$> readFile "inputs/10.txt"

parser :: Parser [List]
parser =
  many $
    choice
      [ List Angle <$> between (char '<') (char '>') parser,
        List Curved <$> between (char '(') (char ')') parser,
        List Curly <$> between (char '{') (char '}') parser,
        List Square <$> between (char '[') (char ']') parser
      ]

mhead (x : _) = Just x
mhead [] = Nothing

maybeLeft (Left x) = Just x
maybeLeft _ = Nothing

errors :: ParseErrorBundle Text Void -> Maybe Char
errors (ParseErrorBundle {bundleErrors = (TrivialError _ (Just (Tokens (x :| _))) _ :| _)}) = Just x
errors _ = Nothing

isClosingError :: ErrorItem Char -> Maybe Char
isClosingError (Tokens (c :| _)) = if c `elem` ("}])>" :: String) then Just c else Nothing
isClosingError _ = Nothing

isEOFError EndOfInput = True
isEOFError _ = False

recov :: ParseError Text Void -> Parser (Maybe Char)
recov (TrivialError _ (Just x) xs) =
  let s = S.insert x xs
      l = S.elems s
      invalid = not $ any isEOFError l
      s' = mapMaybe isClosingError l
      c = head s'
   in if invalid then unexpected x else return (Just c)

parser2 :: Parser [Maybe Char]
parser2 =
  fmap concat $
    many $
      choice
        [ liftA2 (flip (:)) (char '<' *> parser2) (withRecovery recov $ char '>' *> return Nothing),
          liftA2 (flip (:)) (char '(' *> parser2) (withRecovery recov $ char ')' *> return Nothing),
          liftA2 (flip (:)) (char '{' *> parser2) (withRecovery recov $ char '}' *> return Nothing),
          liftA2 (flip (:)) (char '[' *> parser2) (withRecovery recov $ char ']' *> return Nothing)
        ]

parser2' :: Parser [Char]
parser2' = catMaybes <$> parser2

score2 :: String -> Int
score2 (x : xs) = subscore x + (score2 xs * 5)
  where
    subscore ')' = 1
    subscore ']' = 2
    subscore '}' = 3
    subscore '>' = 4
score2 [] = 0

valid = catMaybes . mapMaybe (fmap errors . maybeLeft)

solution1 = sum . fmap score . catMaybes . mapMaybe (fmap errors . maybeLeft) <$> fmap (runParser parser "<input>") <$> input

justToMaybe (Right x) = Just x
justToMaybe _ = Nothing

solution2 = do
  is <- input
  let results = mapMaybe justToMaybe $ do
        i <- is
        let s = runParser parser2' "<input>" i
        return s
  return $ sort (score2 <$> results) !! (length results `div` 2)
