module Day5(day5) where

import Utils
import Data.Bimap (Bimap)
import Data.Bimap qualified as M


parse :: [String] -> ([Rule], [[Page]])
parse s = (numbers2 <$> head ps, numbers <$> (ps!!1))
  where
    ps = splitOn "" s


type Page = Int
type Rule = (Int, Int)


isOrdered :: [Rule] -> Bimap Page Int -> Bool
isOrdered rs pages = all (`checkRule` pages) rs


checkRule :: Rule -> Bimap Page Int -> Bool
checkRule (lo,hi) pages = not (lo `M.member` pages)
                       || not (hi `M.member` pages)
                       || (pages M.! lo < pages M.! hi)


centre :: Bimap Page Int -> Page
centre pages = pages M.!> (M.size pages `div` 2)


-- Keep sorting until no swaps are made
sortPages :: [Rule] -> Bimap Page Int -> Bimap Page Int
sortPages rules = steadyState swapRules
  where
    swapRules :: Bimap Page Int -> Bimap Page Int
    swapRules m = foldl' (\mp rule -> if checkRule rule mp then
                                        mp else
                                        swapPages rule mp
                         ) m rules


swapPages :: Rule -> Bimap Page Int -> Bimap Page Int
swapPages (lo, hi) pages = M.insert hi (pages M.! lo) $ M.insert lo (pages M.! hi) pages


day5 :: IO ()
day5 = do
  ss <- getLines 5
  let (rules, updates) = parse ss
      updateMaps = (\ps -> M.fromList $ zip ps [0..]) <$> updates
      (ordered, notOrdered) = partition (isOrdered rules) updateMaps
      fixed = sortPages rules <$> notOrdered

  putStrLn $ "Day5: part1: " ++ show (sum $ centre <$> ordered)
  putStrLn $ "Day5: part2: " ++ show (sum $ centre <$> fixed)

  return ()
