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


-- Simple but a bit slow (4sec)
countLoops :: (Coord, Coord) -> (Int, Int) -> HashSet Coord -> [Coord]
countLoops pd bounds obstacles = filter (\newObs -> isLoop S.empty pd bounds (newObs `S.insert` obstacles)) visited
  where
    visited = patrol bounds obstacles pd


-- HashSet for the lookups is a bit quicker
isLoop:: HashSet (Coord, Coord) -> (Coord, Coord) -> Coord -> HashSet Coord -> Bool
isLoop setVisited start (rows, cols) obstacles = go setVisited start
  where
    go visited (p@(px,py),d)
      | (p, d) `S.member` visited = True
      | px<0 || px==cols || py<0 || py==rows = False
      | (p+d) `S.member` obstacles = go visited (p, clockTurn d)
      | otherwise = go (S.insert (p, d) visited) (p+d,d)


patrol2 :: (Int, Int) -> HashSet Coord -> ((Int, Int), Coord) -> Int
patrol2 bounds@(rows, cols) obstacles start = S.size $ go S.empty S.empty start
  where
    go visited loops (p@(x,y), d)
      | x<0 || x==cols || y<0 || y==rows = loops
      | nextPos `S.member` obstacles = go visited loops (p, clockTurn d)
      | otherwise = go ((p,d) `S.insert` visited) (if allowed nextPos && nextIsLoop then nextPos `S.insert` loops else loops) (nextPos, d)
      where
        nextPos = p+d
        -- Not out of bounds and not visited
        allowed p@(x,y) = (x>=0) && (y>=0) && (x<cols) && (y<rows)
                        && (p `notElem` (fst <$> S.toList visited))
        nextIsLoop = isLoop visited (p,d) bounds (nextPos `S.insert` obstacles) 


day6 :: IO ()
day6 = do
  ss <- getF lines 6
  let (start, obstacles, size) = parse ss
      l1 = countLoops (start, up) size obstacles
      l2 = patrol2 size obstacles (start, up)
      route = patrol size obstacles (start, up)
      rd = (\(p,from) -> (p, p-from)) <$> zip (tail route) route

  putStrLn $ "Day6: part1: " ++ show (length route)
  putStrLn $ "Day6: part1: " ++ show (patrol2 size obstacles (start, up))

  return ()
