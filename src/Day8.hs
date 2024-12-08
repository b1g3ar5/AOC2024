module Day8(day8) where

import Utils
import Data.List.NonEmpty (groupWith, toList)


inbounds :: Coord -> Bool
inbounds (x,y) = x>=0 && y>=0 && x<50 && y<50


makeNodes:: (Coord -> Coord -> [Coord]) -> [[Coord]] -> [[Coord]]
makeNodes nodeFunc = (filter inbounds <$>) . (go [] <$>)
  where
    go :: [Coord] -> [Coord] -> [Coord]
    go acc [] = acc
    go acc [_] = acc
    go acc (y:ys) = go (acc ++ concatMap (nodeFunc y) ys) ys


nodes1 :: Coord -> Coord -> [Coord]
nodes1 (x1,y1) (x2,y2) =  [(2*x2-x1, 2*y2-y1), (2*x1-x2, 2*y1-y2)]


nodes2 :: Coord -> Coord -> [Coord]
nodes2 p q = (\n -> p + scale n (diff p q )) <$> [(-50)..50]


-- Calculates the lowest divisor of the vector between 2 points
diff :: Coord -> Coord -> Coord
diff p q = (dx `div` d, dy `div` d)
  where
    (dx,dy) = p-q
    d = gcd dx dy


day8 :: IO ()
day8 = do
  ss <- getF lines 8
  let grid :: [(Coord, Char)]
      grid = filter ((/= '.').snd) (parseGridWith id ss)
      
      -- Extract the Coords for each frequency
      g' = toList . (snd <$>) <$> groupWith fst (sortOn fst $ swap <$> grid)

  putStrLn $ "Day8: part1: " ++ show (length $ nub $ concat $ makeNodes nodes1 g')
  putStrLn $ "Day8: part2: " ++ show (length $ nub $ concat $ makeNodes nodes2 g')

  return ()
