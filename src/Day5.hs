module Day5(day5) where

import Utils


parse :: String -> Int
parse = read


day5 :: IO ()
day5 = do
  ss <- getLines 5
  let g = parse <$> ss

  putStrLn $ "Day5: part1: " ++ show g
  putStrLn $ "Day5: part2: " ++ show ""

  return ()
