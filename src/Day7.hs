module Day7(day7) where

import Prelude hiding ((||))
import Utils ( getLines, hylo, splitOn, intercalate )
import Data.List.NonEmpty qualified as N


parse :: String -> (Int, N.NonEmpty Int)
parse s = (read $ head ps, read <$> N.fromList (words (ps!!1)))
  where
    ps = splitOn ':' s


data RTree a  r = Node1 a [r] deriving (Functor, Show)


-- In the backwards tree we apply the inverse of the operations
-- to the target to get a smaller target for each of the children
-- this prunes the tree quite a bit.
-- There will only be a multiplication child node if the target
-- is divisible by the element (first on the remaining list)
-- Also there will only be a concatenation node if the element
-- is there on the end of the target.
coalgB :: Bool -> (Int, N.NonEmpty Int) -> RTree Int (Int, N.NonEmpty Int)
coalgB isPart2 (t, x N.:| xs)
  | null xs = Node1 (if t==x then t else 0) []
  | t < x = Node1 0 []
  | otherwise = Node1 t ns
  where
    (q,r) = t `quotRem` x
    nxs = N.fromList xs -- null xs excluded by the first guard
    ns = (t-x, nxs) : [(q, nxs) | r==0] ++ [(unConcat t x, nxs) | isPart2]


unConcat :: Int -> Int -> Int
unConcat t x
  | t<=x = 0 
  | otherwise = go (reverse $ show t) (reverse $ show x)
  where
    -- Takes digits off one at a time if they are the same
    go [] _ = 0
    go tt [] = read $ reverse tt
    go (tt:ts) (xx:xs)= if tt==xx then go ts xs else 0


-- For backwards the algebra is easy - return the nodes 
-- target only if one of the children suceeded in making
-- the sub target
algB :: RTree Int Int -> Int
algB (Node1 n []) = n -- a leaf
algB (Node1 n xs) = if any (/=0) xs then n else 0


printTree :: RTree Int String -> String
printTree (Node1 x xs) = "Node1: " ++ show x ++ ": " ++ intercalate ", " xs


checkB :: Bool -> Int -> N.NonEmpty Int -> Int
checkB isPart2 target xs = hylo algB (coalgB isPart2) (target, N.reverse xs)


day7 :: IO ()
day7 = do
  ss <- getLines 7
  let g = parse <$> ss

  putStrLn $ "Day7: part1: " ++ show (sum $ uncurry (checkB False) <$> g)
  putStrLn $ "Day7: part2: " ++ show (sum $ uncurry (checkB True) <$> g)

  return ()

-- Forwards tree, much slower than backwards
data Tree a  r = Leaf a | Node a r deriving (Functor, Show)

-- The Tree goes forwards, so the coalgebra is easy
-- just put the numbers in the nodes
coalgF :: [Int] -> Tree Int [Int]
coalgF [] = error "I don't think we should get here"
coalgF [x] = Leaf x
coalgF (x:xs) = Node x xs

-- Maybe I should use logBase 10?
(||) :: Int -> Int -> Int
(||) x i = (10^length (show x))*i + x


-- The algrebra applies the ops to the node values
-- and concatenates all the results
algF :: [Int -> Int -> Int] -> Tree Int [Int] -> [Int]
algF _ (Leaf x) = [x]
algF ops (Node x xs) = ($ x) <$> ops <*> xs



test = ["190: 10 19"
  , "3267: 81 40 27"
  , "83: 17 5"
  , "156: 15 6"
  , "7290: 6 8 6 15"
  , "161011: 16 10 13"
  , "192: 17 8 14"
  , "21037: 9 7 18 13"
  , "292: 11 6 16 20"]