module Day4(day4) where

import Utils
import Data.Map (Map)
import Data.Map qualified as M


-- Instead of checking bounds - use default lookup not in "XMAS"
(!?) :: Ord k => Map k Char -> k -> Char
(!?) mp ix = M.findWithDefault '*' ix mp


find1 :: Map Coord Char -> Int
find1 mp = go 0 $ M.keys mp
  where
    go acc [] = acc
    go acc (ix:ixs) = go (acc + n) ixs
      where
        n = length $ filter (=="XMAS") $ wd ix <$> directions8

        wd :: Coord -> Coord -> String
        wd pos dir = (mp !?) <$> [pos, pos+dir, pos+dir+dir, pos+dir+dir+dir]


find2 :: Map Coord Char -> Int
find2 mp = length $ filter id $ go <$> M.keys mp
  where
    go (x,y) = (mp M.! (x,y) == 'A') && p
      where
        p = [mp !? (x+1,y+1), mp !? (x-1,y-1), mp !? (x+1,y-1), mp !? (x-1,y+1)]
            `elem` ["MSMS","MSSM","SMMS","SMSM"]


day4 :: IO ()
day4 = do
  ss <- getLines 4
  let g = M.fromList $ parseGridWith id ss

  putStrLn $ "Day4: part1: " ++ show (find1 g)
  putStrLn $ "Day4: part2: " ++ show (find2 g)

  return ()

