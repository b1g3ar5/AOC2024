{-# LANGUAGE MultiWayIf #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use newtype instead of data" #-}
{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, ParallelListComp #-}

module Day19(day19) where

import Utils
import Data.List
import Data.MemoTrie (memo)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Array (Array, (!), listArray)


parse :: [String] -> ([String], [String])
parse ls = (words $ filter (/=',') (head ls), tail $ tail ls)


solve1 :: [String] -> String -> Bool
solve1 towels = go
  where
    go = memo $ \des ->
           let ts = filter (`isPrefixOf` des) towels
           in (null des || any (\t -> go $ drop (length t) des) ts)


solve2 :: [String] -> String -> Int
solve2 towels = go
  where
    go :: String -> Int
    go = memo $ \design ->
      if null design then 0 else
      sum $ [if towel == design then 1 else go (drop (length towel) design)
            | towel <- filter (`isPrefixOf` design) towels]


day19 :: IO ()
day19 = do
  ss <- getF lines 19
  let g@(towels, designs) = parse ss
      ways = designWays (foldMap toTrie towels) <$> designs

  putStrLn $ "Day19: part1: " ++ show (length $ filter (solve1 towels) designs)
  putStrLn $ "Day19: part1: " ++ show (sum $ solve2 towels <$> designs)
  putStrLn $ "Day19: part1: " ++ show (sum ways)

  return ()



-- The Bool is "Yes, it fits!"
-- The map is over next possible characters
data TrieF r = NodeF !Bool (Map Char r) deriving (Eq, Show)
data Trie = Node !Bool (Map Char Trie) deriving (Show)

--coalg :: String -> TrieF String
--coalg [] = NodeF True M.empty
--coalg (c:cs) = NodeF False (M.singleton c cs)


-- Word to Trie
toTrie :: String -> Trie
toTrie = foldr (\x t -> Node False (M.singleton x t)) (Node True M.empty)


matches :: Trie -> Int -> String -> [Int]
matches (Node b xs) n letters =
  [n | b] ++
  case letters of
    c:cs | Just t <- M.lookup c xs -> matches t (n+1) cs
    _ -> []


instance Semigroup Trie where
  Node x xs <> Node y ys = Node (x || y) (M.unionWith (<>) xs ys)

instance Monoid Trie where
  mempty = Node False M.empty


designWays :: Trie -> String -> Int
designWays t str = memo ! 0
  where
    n = length str
    memo :: Array Int Int
    memo = listArray (0, n)
           [ if i == n then 1 else sum [memo ! j | j <- matches t i suffix]
           | i      <- [0 .. n]
           | suffix <- tails str]

