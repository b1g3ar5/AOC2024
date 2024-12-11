{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day10(day10) where

import Utils (getLines, hylo, neighbours4, parseGridWith, Coord, TreeF(..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

rows, cols :: Int
rows = 45
cols = rows

inbounds :: (Int, Int) -> Bool
inbounds (x,y) = x>=0 && y>=0 && x < cols && y < rows


type Grid = Map Coord Int
type Path = Set Coord


-- hylo is OK here because the grid is not too big and the paths can only
-- be short. It makes the code very readable.

coalg :: (Coord, Grid, Path) -> TreeF (Coord, Int) (Coord, Grid, Path)
coalg (pos, g, p)
  | null ns = NodeF (pos, value) []
  | otherwise = NodeF (pos, value) $ (\n -> (n, g, pos `S.insert` p)) <$> ns  
  where
    value = g M.! pos
    ns = filter (\n -> inbounds n && g M.! n == value + 1) (neighbours4 pos)


-- Put the trailhead coords in a set
alg1 :: TreeF (Coord, Int) (Set Coord) -> Set Coord
alg1 (NodeF (pos, value) ns)
  | null ns = if value == 9 then S.singleton pos else S.empty
  | otherwise = S.unions ns

-- Count 1 for each trailhead reached
alg2 :: TreeF (Coord, Int) Int -> Int
alg2 (NodeF (_, value) ns)
  | null ns = if value == 9 then 1 else 0
  | otherwise = sum ns


day10 :: IO ()
day10 = do
  ss <- getLines 10
  let g :: Map Coord Int
      g = M.fromList $ parseGridWith (\c -> read [c] ) ss
      starts = M.keys $ M.filter (==0) g

  putStrLn $ "Day10: part2: " ++ show (sum $ (\p -> S.size $ hylo alg1 coalg (p, g, S.empty)) <$> starts )
  putStrLn $ "Day10: part2: " ++ show (sum $ (\p -> hylo alg2 coalg (p, g, S.empty)) <$> starts )

  return ()


