module Day2(day2) where

import Utils


parse :: String -> [Int]
parse s = read <$> words s


isSafe1 :: [Int] -> Bool
isSafe1 [] = True
isSafe1 [_] = True
isSafe1 list@(x:y:xs)
  | list /= sort list && list /= sortBy (comparing Down) list = False
  | abs (x-y) < 1 = False
  | abs (x-y) > 3 = False
  | otherwise = isSafe1 (y:xs)


-- Cowards way out? If the lists were very long?
isSafe2 :: [Int] -> Bool
isSafe2 = any isSafe1 . remove1


remove1 :: [a] -> [[a]]
remove1 [] = []
remove1 (x:xs) = xs : ((x :) <$> remove1 xs)


day2 :: IO ()
day2 = do
  ss <- getLines 2
  let g = parse <$> ss

  putStrLn $ "Day2: part1: " ++ show ( length $ filter id $ isSafe1 <$> g)
  putStrLn $ "Day2: part2: " ++ show ( length $ filter id $ isSafe2 <$> g)

  return ()


test = ["7 6 4 2 1"
  , "1 2 7 8 9"
  , "9 7 6 2 1"
  , "1 3 2 4 5"
  , "8 6 4 4 1"
  , "1 3 6 7 9"]