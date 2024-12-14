{- HLINT ignore "Use head" -}

module Day14(day14) where

import Utils

parse :: String -> (Coord, Coord)
parse s = ((p!!0, p!!1), (p!!2, p!!3))
  where
    p = numbersSigned s


rows, cols, middleRow, middleCol :: Int
rows = 103
cols = 101
middleRow = rows `div` 2
middleCol = cols `div` 2


move :: Int -> (Coord, Coord) -> (Coord, Coord)
move n ((x,y), dp@(dx, dy)) = (((x+n*dx) `mod` cols, (y+n*dy) `mod` rows), dp)


score :: [(Coord, Int)] -> Int
score cs = q1*q2*q3*q4
  where
    q1 = sum $ snd <$> filter (\((x,y), _) -> (x < middleCol) && (y < middleRow)) cs
    q2 = sum $ snd <$> filter (\((x,y), _) -> x > middleCol && y < middleRow) cs
    q3 = sum $ snd <$> filter (\((x,y), _) -> x < middleCol && y > middleRow) cs
    q4 = sum $ snd <$> filter (\((x,y), _) -> x > middleCol && y > middleRow) cs


render :: [Coord] -> [String]
render positions = (\x -> (\y -> if (x,y) `elem` positions then '*' else '.') <$> [0..(rows-1)]) <$> [0..(cols-1)]


day14 :: IO ()
day14 = do
  ss <- getF lines 14
  let rs = parse <$> ss
      positions n = (\g -> (head g, length g)) <$> group (sort $ fst . move n <$> rs)
      treeIx = fst $ minimumBy (comparing snd) $ (\n -> (n, score $ positions n)) <$> [5000..7000]
      tree = fst <$> filter (\((x,y), _) -> x > middleCol && y > middleRow) (positions treeIx)

  putStrLn $ "Day14: part1: " ++ show (score $ positions 100)
  putStrLn $ "Day14: part1:\n " ++ intercalate "\n" (transpose $ drop 50 (drop 50 <$> render tree))
  putStrLn $ "Day14: part2: " ++ show treeIx
  
  return ()
