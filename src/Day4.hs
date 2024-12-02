module Day4(day4) where

import Utils


parse :: String -> Int
parse = read


day4 :: IO ()
day4 = do
  ss <- getLines 4
  let g = parse <$> ss

  putStrLn $ "Day4: part1: " ++ show g
  putStrLn $ "Day4: part2: " ++ show ""

  return ()
