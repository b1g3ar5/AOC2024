module Day7(day7) where

import Utils


parse :: String -> (Int, [Int])
parse s = (read $ head ps, read <$> words (ps!!1))
  where
    ps = splitOn ':' s


check1 :: Int -> [Int] -> Int
check1 target = go []
  where
    go :: [Int] -> [Int] -> Int
    go acc [] = if target `elem` acc then target else 0
    go [] (x:xs) = go [x] xs
    go acc [x] = go ([(x +), (x *)] <*> acc) [] 
    go acc (x:xs) = go ([(x +), (x *)] <*> acc)  xs
    

check2 :: Int -> [Int] -> Int
check2 target = go []
  where
    (||) x i = (10^length (show x))*i + x 

    go :: [Int] -> [Int] -> Int
    go acc [] = if target `elem` acc then target else 0
    go acc [x] = go ([(x +), (x * ), (x ||)] <*> acc) [] 
    go [] (x:xs) = go [x] xs
    go acc (x:xs) = go ([(x + ), (x *), (x ||)] <*> acc)  xs
    

day7 :: IO ()
day7 = do
  ss <- getLines 7
  let g = parse <$> ss

  putStrLn $ "Day7: part1: " ++ show (sum $ uncurry check1 <$> g)
  putStrLn $ "Day7: part2: " ++ show (sum $ uncurry check2 <$> g)

  return ()

