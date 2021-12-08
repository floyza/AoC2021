module Lib where

-- cyc lst = go lst
--   where
--     go (x : xs) = x : go xs
--     go [] = go lst

cyc :: Semigroup a => a -> a
cyc a = let a' = a <> a' in a'
