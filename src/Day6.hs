module Day6(day6) where

import Utils


parse :: String -> Int
parse = read


day6 :: IO ()
day6 = do
  ss <- getLines 6
  let g = parse <$> ss

  putStrLn $ "Day6: part1: " ++ show g
  putStrLn $ "Day6: part2: " ++ show ""

  return ()
