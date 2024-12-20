module Day20(day20) where

import Utils
import Data.PQueue.Prio.Min (MinPQueue(..))
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Data.MemoTrie ((:->:)(BoolTrie))


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
    -- There is only one path...
    next :: Coord -> Map Coord Time -> Coord
    next p seen = head $ filter (\n -> not (n `S.member` walls) && not (n `M.member` seen)) $ neighbours4 p

    go :: (Coord, Int) -> Map Coord Time -> Map Coord Time
    go (p, t) seen = if p == end then M.insert p t seen else go (next p seen, t+1) (M.insert p t seen)
      

savingThreshold :: Time
savingThreshold = 100
rows :: Int
rows = 141


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
      ks = M.keys timeMap

      --xx :: Map Time Freq
      x2 = foldl (\mp from -> foldl (go 2 from) mp ks ) M.empty ks
      x20 = foldl (\mp from -> foldl (go 20 from) mp ks ) M.empty ks

      go :: Int -> Coord -> Map Time Freq -> Coord -> Map Time Freq
      go cheatLength from mp to
        | dist > cheatLength = mp
        | saving > savingThreshold = M.insertWith (+) saving 1 mp
        | otherwise = mp
        where
          dist = manhattan from to
          saving = timeToEndMap M.! from  - timeToEndMap M.! to - dist + 1


  putStrLn $ "Day20: part1: " ++ show (sum x2)
  putStrLn $ "Day20: part1: " ++ show (sum x20)

  return ()
