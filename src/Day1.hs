module Day1(day1) where

import Utils
import Data.Map as M


parse :: String -> (Int, Int)
parse s = (read $ p!!0, read $ p!!1)
  where
    p = words s


day1 :: IO ()
day1 = do
  ss <- getLines 1
  let (list1, list2) = bimap sort sort $ unzip $ parse <$> ss
      map2 :: M.Map Int Int
      map2 = M.fromList $ (\gp -> (head gp, length gp)) <$> group list2

  putStrLn $ "Day1: part1: " ++ show (sum $ zipWith (\x y -> abs $ x-y) list1 list2)
  putStrLn $ "Day1: part2: " ++ show (sum $ (\x -> x * M.findWithDefault 0 x map2) <$> list1)

  return ()
