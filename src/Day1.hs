module Day1(day1) where

import Utils
import TotalMap qualified as T
import TotalMap()


day1 :: IO ()
day1 = do
  ss <- getLines 1
  let (list1, list2) = bimap sort sort $ unzip $ numbers2 <$> ss
      frequency :: T.TMap Int Int
      frequency = T.fromList 0 $ (\gp -> (head gp, length gp)) <$> group list2

  putStrLn $ "Day1: part1: " ++ show (sum $ zipWith (\x y -> abs $ x-y) list1 list2)
  putStrLn $ "Day1: part2: " ++ show (sum $ (\x -> x * frequency T.! x) <$> list1)

  return ()
