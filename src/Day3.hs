module Day3(day3) where

import Utils


parse :: String -> Int
parse = read


day3 :: IO ()
day3 = do
  ss <- getLines 3
  let g = parse <$> ss

  putStrLn $ "Day3: part1: " ++ show g
  putStrLn $ "Day3: part2: " ++ show ""

  return ()
