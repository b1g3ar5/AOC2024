module Day6(day6) where

import Utils
import Data.HashSet (HashSet)
import Data.HashSet qualified as H
import Data.Set qualified as S



parse :: [String] -> (Coord, HashSet Coord, Coord)
parse ss = (start, obstacles, size)
  where
    obstacles = H.fromList $ (fst <$>) . filter snd . parseGridWith (=='#') $ ss
    start = head $ (fst <$>) . filter snd .parseGridWith (=='^') $ ss
    size = (length ss, length $ head ss)


patrol1 :: Coord -> HashSet Coord -> (Coord, Coord) -> [Coord]
patrol1 (rows, cols) obstacles = go []
  where
    go acc (p@(x,y), d)
      | x<0 || x==cols || y<0 || y==rows = acc
      | (p+d) `H.member` obstacles = go acc (p, clockTurn d)
      | otherwise = go (p:acc) (p+d, d)


isLoop:: HashSet (Coord, Coord) -> (Coord, Coord) -> Coord -> HashSet Coord -> Bool
isLoop visited (p@(px,py),d) (rows, cols) obstacles
  | (p, d) `H.member` visited = True
  | px<0 || px==cols || py<0 || py==rows = False
  | (p+d) `H.member` obstacles = isLoop visited (p, clockTurn d) (rows, cols) obstacles
  | otherwise = isLoop (H.insert (p, d) visited) (p+d,d) (rows, cols) obstacles


-- Places an obstacle in front of the path to see if it's a loop
-- on every move
patrol2 :: (Int, Int) -> HashSet Coord -> (Coord, Coord) -> Int
patrol2 bounds@(rows, cols) obstacles start = H.size $ go H.empty H.empty start
  where
    go visited loops (p@(x,y), d)
      | x<0 || x==cols || y<0 || y==rows = loops
      | nextPos `H.member` obstacles = go visited loops (p, clockTurn d)
      | otherwise = go ((p,d) `H.insert` visited) (if allowed nextPos && nextIsLoop then nextPos `H.insert` loops else loops) (nextPos, d)
      where
        nextPos = p+d
        -- Not out of bounds and not visited
        allowed pp@(xx,yy) = (xx>=0) && (yy>=0) && (xx<cols) && (yy<rows)
                           && (pp `notElem` (fst <$> H.toList visited))
        nextIsLoop = isLoop visited (p,d) bounds (nextPos `H.insert` obstacles) 


day6 :: IO ()
day6 = do
  ss <- getF lines 6
  let (start, obstacles, size) = parse ss
      p1 = patrol1 size obstacles (start, up)

  putStrLn $ "Day6: part1: " ++ show (length $ nub p1 )
  putStrLn $ "Day6: part2: " ++ show (patrol2 size obstacles (start, up))

  return ()
