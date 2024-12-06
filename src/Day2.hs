module Day2(day2) where

import Utils


parse :: String -> [Int]
parse s = read <$> words s


isSafe :: [Int] -> Bool
isSafe list
  | list /= sort list && list /= sortBy (comparing Down) list  = False
  | otherwise = checkDifferences list
  where
    checkDifferences [] = True
    checkDifferences [_] = True
    checkDifferences (x:y:xs)
      | abs (x-y) > 3 = False
      | abs (x-y) < 1 = False
      | otherwise = checkDifferences (y:xs)


-- All the lists with one item removed
remove1 :: [a] -> [[a]]
remove1 [] = []
remove1 (x:xs) = xs : ((x :) <$> remove1 xs)


day2 :: IO ()
day2 = do
  ss <- getLines 2
  let g = parse <$> ss

  putStrLn $ "Day2: part1: " ++ show ( length $ filter id $ isSafe <$> g)
  putStrLn $ "Day2: part2: " ++ show ( length $ filter id $ any isSafe . remove1 <$> g)

  return ()