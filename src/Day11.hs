
{-# LANGUAGE MultiWayIf #-}

module Day11(day11) where

import Utils (times)
import Data.IntMap qualified as M
import Data.MemoTrie (memo2)


parse :: String -> [Int]
parse = (read <$>) .  words


-- Takes the steps and a stone and returns the number of stones
changeStone :: Int -> Int  -> Int
changeStone  = memo2 $ \steps stone -> 
  let stoneString = show stone
      len = length stoneString
      (leftShone, rightStone) = stone `quotRem` (10 ^ (len `quot` 2))
  in if
  | steps == 0 -> 1
  | stone == 0 -> changeStone (steps-1) 1
  | even len   -> changeStone (steps-1) leftShone 
                + changeStone (steps-1) rightStone
  | otherwise  -> changeStone (steps-1) (2024 * stone)

day11 :: IO ()
day11 = do
  let s = "0 44 175060 3442 593 54398 9 8101095"
  let g = parse s

  putStrLn $ "Day11: part1: " ++ show (sum $ memo2 changeStone 25 <$> g)
  putStrLn $ "Day11: part1: " ++ show (sum $ memo2 changeStone 75 <$> g)
  --putStrLn $ "Day11: part1: " ++ show (sum $ times 25 nextMap $ M.fromList $ (,1) <$> g)
  --putStrLn $ "Day11: part2: " ++ show (sum $ times 100 nextMap $ M.fromList $ (,1) <$> g)

  return ()


type StoneCount = M.IntMap Int

-- Keep a count of each time a stone/number occurs
-- so we only calculate once...
nextMap :: StoneCount -> StoneCount
nextMap sc = M.fromListWith (+) [(s', n) | (s,n) <- M.assocs sc, s' <- nextStone s]


nextStone :: Int -> [Int]
nextStone 0 = [1]
nextStone s
  | even len  = [leftShone, rightStone]
  | otherwise = [2024 * s]
  where
    ss = show s
    len = length ss
    (leftShone, rightStone) = s `quotRem` (10 ^ (len `quot` 2))
