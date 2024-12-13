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
stats2 cs = area * cornersScore
  where
    area = length cs
    -- A corner should really have coords at half way
    -- but I count (x,y) as the corner at (x-0.5, y-0.5)
    -- Each cell hae 4 corners
    corners :: Coord -> [Coord]
    corners (x,y) = [(x-1, y), (x,y), (x-1, y-1), (x,y-1)]
    -- This operates on the corner groups
    cornersScore = sum $ (\g -> if
                         | length g == 1 -> 1 -- A corner appearing once is a convex corner
                         | length g == 3 -> 1 -- A corner with 3 cells round it is an concave corner
                         | length g == 2      -- A corner appearing twice counts twice if the 
                           && isDiagonal (fst <$> g) -> 2 -- neighbouring cells are diagonal
                         | otherwise -> 0 -- Otherwise it's not a real corner
                 ) <$> allCorners
    -- This is all the corners of all the cells grouped by corner
    -- So the length of the group is the number of cells surrounding the corner                 
    allCorners = groupBy (\a b -> snd a == snd b) (sortOn snd (concatMap (\c -> (c,) <$> corners c) cs))

    isDiagonal :: [Coord] -> Bool
    isDiagonal [(x1, y1), (x2, y2)] = (x1 /= x2) && (y1 /= y2)
    isDiagonal _ = False


getRegion :: Set (Coord, Char) -> Set (Coord, Char)
getRegion cs = dfsSet next (S.singleton $ 0 `S.elemAt` cs)
  where
    next :: (Coord, Char) -> S.Set (Coord, Char)
    next (pos, c) = S.fromList $ ((,c) <$> ) <$> filter (\p -> ((p,c) `elem` cs) && inbounds p) $ neighbours4 pos


-- This works as follows:
-- Get all the plant types
-- Sort into all coords for each plant type
-- Sorts these into connected regions
getRegions :: Set (Coord, Char) -> [Set (Coord, Char)]
getRegions cs = if null remaining then S.toList rs else S.toList rs ++ getRegions remaining
  where
    remaining = cs S.\\ S.unions rs
    plants = S.map snd cs
    regions = S.map (\p -> S.filter ((==p) . snd) cs) plants
    rs = S.map getRegion regions


day12 :: IO ()
day12 = do
  ss <- getF lines 12
  --let ss = test3
  let g = parseGridWith id ss
      regions = getRegions $ S.fromList g

  putStrLn $ "Day12: part2: " ++ show ( sum $ stats1 . S.map fst <$> regions)
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