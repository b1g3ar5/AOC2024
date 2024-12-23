module Day20(day20) where

import Utils
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M


parse :: [String] -> [Coord]
parse = (fst <$>) . filter snd . parseGridWith (=='#')

parseStart, parseEnd :: [String] -> Coord
parseStart = head . (fst <$>) . filter snd . parseGridWith (=='S')
parseEnd  =  head . (fst <$>) . filter snd . parseGridWith (=='E')


type Time = Int
type Freq = Int


solve :: Coord -> Coord -> Set Coord -> Map Coord Time
solve start end walls = go (start, 0) M.empty
  where 
    -- There is only one path - using head
    next :: Coord -> Map Coord Time -> Coord
    next p seen = head $ filter (\n -> not (n `S.member` walls) && not (n `M.member` seen)) $ neighbours4 p

    go :: (Coord, Int) -> Map Coord Time -> Map Coord Time
    go (p, t) seen = if p == end then M.insert p t seen else go (next p seen, t+1) (M.insert p t seen)
      

savingThreshold :: Time
savingThreshold = 100


-- Simple (slow) algorithm - run over all pairs of coords in the path
-- see if the cheat between them is worth it - if so increment the counter
countCheats :: Map Coord Time -> Time -> Int
countCheats timeMap cheatTime = foldl (\mp from -> foldl (go cheatTime from) mp ks ) 0 ks
  where
    ks = M.keys timeMap
    go :: Int -> Coord -> Int -> Coord -> Int
    go cheatLength from counter to
      | dist > cheatLength = counter
      | saving >= savingThreshold = counter+1
      | otherwise = counter
      where
        dist = manhattan from to
        saving = timeMap M.! from  - timeMap M.! to - dist


day20 :: IO ()
day20 = do
  ss <- getF lines 20
  let walls = S.fromList $ parse ss
      start = parseStart ss
      end = parseEnd ss

      timeMap :: Map Coord Time
      timeMap = solve start end walls

      minTime :: Time
      minTime = maximum timeMap

      timeToEndMap :: Map Coord Time
      timeToEndMap = (minTime-) <$> timeMap


  putStrLn $ "Day20: part1: " ++ show (countCheats timeToEndMap 2)
  putStrLn $ "Day20: part2: " ++ show (countCheats timeToEndMap 20)

  return ()
