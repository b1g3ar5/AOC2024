module Day7(day7) where

import Prelude hiding ((||))
import Prelude qualified as P
import Utils ( getLines, hylo, ana, splitOn, intercalate )


parse :: String -> (Int, [Int])
parse s = (read $ head ps, read <$> words (ps!!1))
  where
    ps = splitOn ':' s


-- Maybe I should use logBase 10?
(||) :: Int -> Int -> Int
(||) x i = (10^length (show x))*i + x


-- Not sure we need a Tree...
data Tree a  r = Leaf a | Node a r deriving (Functor, Show)
data RTree a  r = Node1 a [r] deriving (Functor, Show)


-- The Tree goes forwards, so the coalgebra is easy
-- just put the numbers in the nodes
coalgF :: [Int] -> Tree Int [Int]
coalgF [] = error "I don't think we should get here"
coalgF [x] = Leaf x
coalgF (x:xs) = Node x xs


-- The algrebra applies the ops to the node values
-- and concatenates all the results
algF :: [Int -> Int -> Int] -> Tree Int [Int] -> [Int]
algF _ (Leaf x) = [x]
algF ops (Node x xs) = ($ x) <$> ops <*> xs


-- In the backwards tree we apply the inverse of the operations
-- to the target to get a smaller target for each of the children
-- So, there will only be a multiplication child node if the target
-- is divisible by the element (first on the remaining list)
-- Also there will only be a concatenation node if the element
-- is there on the end of the target.
coalgB :: Bool -> (Int, [Int]) -> RTree Int (Int, [Int])
coalgB _ (_,[]) = error "I don't think we should get here"
coalgB _ (t, [x]) = Node1 (if t==x then t else 0) []
coalgB isPart2 (t, x:xs)
  | t<x = Node1 0 []
  | otherwise = Node1 t ns
  where
    (q,r) = t `quotRem` x
    ns = (t-x, xs) : [(q,xs) | r==0] ++ [(unConcat t x, xs) | isPart2]


-- take the second number off the end of the first
unConcat :: Int -> Int -> Int
unConcat t x
  | t==x = 0
  | otherwise = go (reverse $ show t) (reverse $ show x)
  where
    go [] _ = 0
    go tt [] = read $ reverse tt
    go (tt:ts) (xx:xs)= if tt==xx then go ts xs else 0


-- For backwards the algebra is easy - return the nodes 
-- target only if one of the children suceeded in making
-- the target
algB :: RTree Int Int -> Int
algB (Node1 n []) = n -- a leaf
algB (Node1 n xs) = if any (/=0) xs then n else 0


printTree :: RTree Int String -> String
printTree (Node1 x xs) = "Node1: " ++ show x ++ ": " ++ intercalate ", " xs


-- The forward check sees if the target is anywhere in the
-- set of results  
checkF :: [Int ->Int -> Int] -> Int -> [Int] -> Int
checkF ops target xs = if target `elem` res then target else 0
  where
    res = hylo (algF ops) coalgF $ reverse xs


checkB :: Bool -> Int -> [Int] -> Int
checkB isPart2 target xs = hylo algB (coalgB isPart2) (target, reverse xs)


day7 :: IO ()
day7 = do
  ss <- getLines 7
  let g = parse <$> ss

  --putStrLn $ "Day7: part1: " ++ show (sum $ uncurry (checkF [(+), (*)]) <$> g)
  --putStrLn $ "Day7: part2: " ++ show (sum $ uncurry (checkF [(+), (*), (||)]) <$> g)
  putStrLn $ "Day7: part2: " ++ show (sum $ uncurry (checkB False) <$> g)
  putStrLn $ "Day7: part2: " ++ show (sum $ uncurry (checkB True) <$> g)

  return ()


test = ["190: 10 19"
  , "3267: 81 40 27"
  , "83: 17 5"
  , "156: 15 6"
  , "7290: 6 8 6 15"
  , "161011: 16 10 13"
  , "192: 17 8 14"
  , "21037: 9 7 18 13"
  , "292: 11 6 16 20"]