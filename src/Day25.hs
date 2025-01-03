module Day25(day25) where

import Utils


parseKey :: [String] -> (Bool, [Int])
parseKey ss = (head (ss!!0) == '#', length . filter (=='#') <$> transpose ss)
  

parse :: [String] -> [(Bool, [Int])]
parse s = parseKey <$> ps
  where
    ps = splitOn "" s


isFit :: [Int] -> [Int] -> Bool
isFit k l = 5 == length (filter (<=7) $ zipWith (+) k l)

day25 :: IO ()
day25 = do
  ss <- getLines 25
  let xs = parse $ ss
      (keys, locks) = bimap (snd <$>) (snd <$>) $ partition fst xs

  putStrLn $ "Day25: part1: " ++ show (length [ () | k <- keys, l <- locks, isFit k l])

  return ()
