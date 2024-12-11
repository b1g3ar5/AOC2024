module Day4(day4) where

import Utils
import TotalMap (TMap)
import TotalMap qualified as T

-- Total map is a map with a default element, so lookup always succeeds

find1 :: TMap Coord Char -> Int
find1 mp = go 0 $ T.keys mp
  where
    go acc [] = acc
    go acc (ix:ixs) = go (acc + n) ixs
      where
        -- Check for XMAS in all 8 directions
        n = length $ filter (=="XMAS") $ getWord ix <$> directions8

        -- Get the word from a position in a direction
        -- No need to check bounds because of the TMap
        getWord :: Coord -> Coord -> String
        getWord pos dir = (mp T.!) <$> [pos, pos+dir, pos+dir+dir, pos+dir+dir+dir]


find2 :: TMap Coord Char -> Int
find2 mp = length $ filter check $ T.keys mp
  where
    check (x,y) = (mp T.! (x,y) == 'A') && p
      where
        -- 4 alternatives for the wings of the cross
        p = [mp T.! (x+1,y+1), mp T.! (x-1,y-1), mp T.! (x+1,y-1), mp T.! (x-1,y+1)]
            `elem` ["MSMS","MSSM","SMMS","SMSM"]


day4 :: IO ()
day4 = do
  ss <- getLines 4
  let -- Make a map with * as the default element
      g = T.fromList '*' $ parseGridWith id ss

  putStrLn $ "Day4: part1: " ++ show (find1 g)
  putStrLn $ "Day4: part2: " ++ show (find2 g)

  return ()

