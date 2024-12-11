module Day11(day11) where

import Utils (times, timeIt, swap)
import Data.IntMap qualified as M
import Data.MemoTrie


parse :: String -> [Int]
parse = (read <$>) .  words


-- Using IntMap - the frequency of the stone
type StoneCount = M.IntMap Int


-- Keep a count of each time a stone/number occurs
-- so we only calculate once...
nextMap :: StoneCount -> StoneCount
nextMap sc = M.fromListWith (+) [ (s', n) | (s,n) <- M.assocs sc, s' <- nextStone s]


nextStone :: Int -> [Int]
nextStone 0 = [1]
nextStone s
  | even len  = [leftShone, rightStone]
  | otherwise = [2024 * s]
  where
    ss = show s
    len = length ss
    (leftShone, rightStone) = s `quotRem` (10 ^ (len `quot` 2))


day11 :: IO ()
day11 = do
  let s = "0 44 175060 3442 593 54398 9 8101095"
  let g = parse s

  putStrLn $ "Day11: part1: " ++ show (sum $ times 25 nextMap $ M.fromList $ (,1) <$> g)
  putStrLn $ "Day11: part2: " ++ show (sum $ times 75 nextMap $ M.fromList $ (,1) <$> g)
  timeIt $ putStrLn $ "Day11: part1: " ++ show (sum $ memo2 changeStone 25 <$> g)
  timeIt $ putStrLn $ "Day11: part1: " ++ show (sum $ memo2 changeStone 75 <$> g)

  return ()


-- Takes the steps and a stone and returns the number of stones
changeStone :: Int -> Int -> Int
changeStone steps stone
  | steps == 0 = 1
  | stone == 0 = memo2 changeStone (steps-1) 1
  | even len  = memo2 changeStone (steps-1) leftShone  + memo2 changeStone (steps-1) rightStone
  | otherwise = memo2 changeStone (steps-1) (2024 * stone)
  where
    stoneString = show stone
    len = length stoneString
    (leftShone, rightStone) = stone `quotRem` (10 ^ (len `quot` 2))

