{-# LANGUAGE MultiWayIf #-}

module Day12(day12) where

import Utils
import Data.Set (Set)
import Data.Set qualified as S

-- Part 2 quite difficult
-- I solved it by finding corners (number of sides == number of corners)
-- but it's complicated code - I wonder if there's an easy way.

rows :: Int
rows = 140

inbounds :: Coord -> Bool
inbounds (x,y) = (x>=0) && (x<rows) && (y>=0) && (y<rows)


stats1 :: Set Coord -> Int
stats1 cs = area * sum (perimeter `S.map` cs)
  where
    area = length cs
    perimeter  = (4 -) . length . filter (\p -> (p `elem` cs) && inbounds p) . neighbours4


stats2 :: Set Coord -> Int
stats2 cs = area * regionCorners
  where
    area = length cs
    -- The 4 corners of a cell
    corners :: Coord -> [Coord]
    corners (x,y) = [(x-1, y), (x,y), (x-1, y-1), (x,y-1)]
    -- For a cell corner to be a region corner it must have:
    -- 1 touching cell for a convex corner
    -- 3 touching cells for a concave corner
    -- 2 touching cells if they are diagonal - in whcih case it counts twice 
    -- otherwise it's not a region corner
    regionCorners = sum $ (\g -> if
                         | length g == 1 -> 1 
                         | length g == 3 -> 1 
                         | length g == 2       
                           && isDiagonal (fst <$> g) -> 2 
                         | otherwise -> 0 
                 ) <$> cellCorners
    -- [[(cell, corner)]] grouped on corner
    cellCorners = groupBy (\a b -> snd a == snd b) (sortOn snd (concatMap (\c -> (c,) <$> corners c) cs))

    isDiagonal :: [Coord] -> Bool
    isDiagonal [(x1, y1), (x2, y2)] = (x1 /= x2) && (y1 /= y2)
    isDiagonal _ = False


getRegion :: Set (Coord, Char) -> [Set (Coord, Char)]
getRegion cs 
  | null cellsRemaining = [region]
  | otherwise = region : getRegion cellsRemaining
  where
    cellsRemaining = cs S.\\ region
    region = dfsSet next (S.singleton $ 0 `S.elemAt` cs)
    next :: (Coord, Char) -> S.Set (Coord, Char)
    next (pos, c) = S.fromList $ ((,c) <$> ) <$> filter (\p -> ((p,c) `elem` cs) && inbounds p) $ neighbours4 pos


getRegions :: Set (Coord, Char) -> [Set (Coord, Char)]
getRegions cs = concat $ S.toList regions
  where
    plants = S.map snd cs -- All the different palnts
    cellsForEachPlant = S.map (\p -> S.filter ((==p) . snd) cs) plants -- The cells for each plant
    regions = S.map getRegion cellsForEachPlant -- Sort into  touching regions


day12 :: IO ()
day12 = do
  ss <- getF lines 12
  --let ss = test3
  let g = parseGridWith id ss
      regions = getRegions $ S.fromList g

  putStrLn $ "Day12: part1: " ++ show ( sum $ stats1 . S.map fst <$> regions)
  putStrLn $ "Day12: part2: " ++ show ( sum $ stats2 . S.map fst <$> regions)

  return ()


test = ["AAAA"
  , "BBCD"
  , "BBCC"
  , "EEEC"]

test1 = ["OOOOO"
  , "OXOXO"
  , "OOOOO"
  , "OXOXO"
  , "OOOOO"]

test2 = ["EEEEE"
  , "EXXXX"
  , "EEEEE"
  , "EXXXX"
  , "EEEEE"]

test3 = ["AAAAAA"
  , "AAABBA"
  , "AAABBA"
  , "ABBAAA"
  , "ABBAAA"
  , "AAAAAA"]