{-# LANGUAGE LambdaCase #-}

module Day18(day18) where

import Utils hiding (dfs)
import Data.PQueue.Prio.Min qualified as Q
import Queue qualified as QQ
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M


parse :: [String] -> [Coord]
parse = (numbers2 <$>)

start, exit :: Coord
start = (0,0)
exit = (rows-1, rows-1)
rows :: Int
rows = 71
usage :: Int
usage = 1024

inbounds :: Coord -> Bool
inbounds (x,y) = (x >= 0) && (y >= 0) && (x < rows) && (y < rows)


solve1 :: S.Set Coord -> Map Coord Int
solve1 bytes = dfs S.empty (Q.fromList [(0, start)])
  where
    next :: Coord -> [Coord]
    next p = filter (\n -> inbounds n && not (n `S.member` bytes))  (neighbours4 p)

    dfs :: S.Set Coord -> Q.MinPQueue Int Coord -> Map Coord Int
    dfs !seen = \case
              Q.Empty -> M.empty
              x@(n,p) Q.:< newq
                | p `S.member` seen -> dfs seen newq
                | otherwise -> M.insert p n $ dfs (p `S.insert` seen) (foldr (Q.insert (n+1)) newq (next p))


solve2 :: [Coord] -> Int
solve2 bytes = go 1024 1000
  where
    failure :: Map Coord Int -> Bool
    failure mp = isNothing $ mp M.!? exit

    go :: Int -> Int -> Int
    go lo step
      | hiFailed  = if step==1 then lo else go lo $ step `div` 2
      | otherwise = go (lo+step) step
      where
        hiFailed = failure $ solve1 (S.fromList $ take (lo+step) bytes)
        

day18 :: IO ()
day18 = do
  --ss <- getTest lines 18
  ss <- getF lines 18
  let g = parse ss
      g1 = S.fromList $ take usage g

  putStrLn $ "Day18: part1: " ++ show ( solve1 g1 M.! exit)
  putStrLn $ "Day18: part2: " ++ show (g !! solve2 g)

  return ()
