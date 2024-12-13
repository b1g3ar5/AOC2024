module Day13(day13) where

import Utils 


type Machine = ((Int, Int),(Int, Int),(Int, Int))


addPrize :: Int -> Machine -> Machine
addPrize n (a,b, (px, py)) = (a,b,(px+n, py+n))


parseMachine :: [String] -> Machine
parseMachine ls = ((a!!0,  a!!1), (b!!0, b!!1), (p!!0, p!!1))
  where
    a = numbers $ ls!!0
    b = numbers $ ls!!1
    p = numbers $ ls!!2


parse :: [String] -> [Machine]
parse ss = parseMachine <$> cs
  where
    cs = chunksOf 4 ss


solveMachine :: Machine -> Maybe Int
solveMachine ((ax, ay), (bx,by), (px, py))
  | det == 0 = error "The buttons are not independent"
  | (rm == 0) && (rn == 0) = Just (3*n+m)
  | otherwise = Nothing
  where
    det = bx*ay-ax*by
    (m, rm) = (px*ay - py*ax) `quotRem` det
    (n, rn) = (py*bx - px*by) `quotRem` det


day13 :: IO ()
day13 = do
  ss <- getF lines 13
  let g1 = parse ss
      g2 = addPrize 10000000000000 <$> g1

  putStrLn $ "Day13: part1: " ++ show (sum (sum . solveMachine <$> g1))
  putStrLn $ "Day13: part2: " ++ show (sum (sum . solveMachine <$> g2))

  return ()
