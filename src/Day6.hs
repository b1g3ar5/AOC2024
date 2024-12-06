module Day6(day6) where

import Utils
import Data.HashSet (HashSet)
import Data.HashSet qualified as S


parse :: [String] -> (Coord, HashSet Coord, Coord)
parse ss = (start, obstacles, size)
  where
    obstacles = S.fromList $ (fst <$>) . filter snd . parseGridWith (=='#') $ ss
    start = head $ (fst <$>) . filter snd .parseGridWith (=='^') $ ss
    size = (length ss, length $ head ss)


patrol :: Coord -> HashSet Coord -> (Coord, Coord) -> [Coord]
patrol (rows, cols) obstacles = go []
  where
    go acc (p@(x,y), d)
      | x<0 || x==cols || y<0 || y==rows = nub acc
      | (p+d) `S.member` obstacles = go acc (p, clockTurn d)
      | otherwise = go (p:acc) (p+d, d)


countLoops :: (Coord, Coord) -> (Int, Int) -> HashSet Coord -> [Coord]
countLoops pd bounds obstacles = filter (\q -> isLoop pd bounds (q `S.insert` obstacles)) visited
  where
    visited = patrol bounds obstacles pd


isLoop:: (Coord, Coord) -> Coord -> HashSet Coord -> Bool
isLoop pd (rows, cols) obstacles = go S.empty pd
  where
    go acc (p@(px,py),d)
      | (p, d) `S.member` acc = True
      | px<0 || px==cols || py<0 || py==rows = False
      | (p+d) `S.member` obstacles = go acc (p, clockTurn d)
      | otherwise = go (S.insert (p, d) acc) (p+d,d)

day6 :: IO ()
day6 = do
  ss <- getLines 6
  let (start, obstacles, size) = parse ss

  putStrLn $ "Day6: part1: " ++ show (length $ patrol size obstacles (start, up))
  putStrLn $ "Day6: part2: " ++ show (length $ countLoops (start, up) size obstacles)

  return ()
