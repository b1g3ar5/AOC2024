module Day22(day22) where

import Utils (getLines, times)
import Data.Bits
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as M


mix :: Int -> Int -> Int
mix a b = a `xor` b


prune :: Int -> Int
prune x = x `mod` 16777216
--prune x = x .&. 16777215


evolve :: Int -> Int
evolve x = x3
  where
    x1 = prune $ mix (x `shiftL` 6) x
    x2 = prune $ mix (x1 `shiftR` 5) x1
    x3 = prune $ mix (x2 `shiftL` 11) x2


-- Make the 2001 prices for a starting value
makePrices :: Int -> [Int]
makePrices a = (`mod` 10) <$> go 2001 a
  where
    go 0 _ = []
    go n x = x : go (n-1) (evolve x)


-- The sequence of differences - to index the map
type Seq = [Int]


-- Given the prices work out all the differences and make a map of
-- the number of bananas for each difference sequence  
--allScores :: [Int] -> Map Seq Int
allScores prices = foldl (\mp (ix,v) -> M.insertWith (const id) ix v mp) M.empty diffsPrice
  where
    -- Make a list of all the differeces and the bananas they would offer
    diffsPrice :: [([Int], Int)]
    diffsPrice = zip (take 4 <$> tails (differences prices)) $ drop 4 prices


differences :: [Int] -> [Int]
differences xs = zipWith (-) xs $ tail xs


-- Make the combined map over all banana sellers
makeMap :: [Int] -> Map Seq Int
--makeMap xs = M.unionsWith (+) $ allScores . makePrices <$> xs
makeMap = foldl (\mp -> M.unionWith (+) mp . allScores . makePrices) M.empty


day22 :: IO ()
day22 = do
  ss <- getLines 22
  let g = read <$> ss

  putStrLn $ "Day22: part2: " ++ show (sum $ times 2000 evolve <$> g)
  putStrLn $ "Day22: part2: " ++ show (maximum $ makeMap g )

  return ()
