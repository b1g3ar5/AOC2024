module Day7(day7) where

import Prelude hiding ((||))
import Utils ( getLines, hylo, splitOn )


parse :: String -> (Int, [Int])
parse s = (read $ head ps, read <$> words (ps!!1))
  where
    ps = splitOn ':' s


(||) :: (Num a, Show a) => a -> a -> a
(||) x i = (10^length (show x))*i + x 


-- Not sure we need a Tree
data Tree a  r = Leaf a | Node a r deriving (Functor, Show)


coalg :: [Int] -> Tree Int [Int]
coalg [] = error "I don't think we should get here"
coalg [x] = Leaf x
coalg (x:xs) = Node x xs


alg :: [Int ->Int -> Int] ->Tree Int [Int] -> [Int]
alg _ (Leaf x) = [x]
alg ops (Node x xs) = ($ x) <$> ops <*> xs 


check :: [Int ->Int -> Int] -> Int -> [Int] -> Int
check ops target xs = if target `elem` res then target else 0
  where
    res = hylo (alg ops) coalg $ reverse xs


day7 :: IO ()
day7 = do
  ss <- getLines 7
  let g = parse <$> ss

  putStrLn $ "Day7: part1: " ++ show (sum $ uncurry (check [(+), (*)]) <$> g)
  putStrLn $ "Day7: part2: " ++ show (sum $ uncurry (check [(+), (*), (||)]) <$> g)

  return ()


