module Day9(day9) where

import Utils
import Data.Sequence (Seq, (|>), (<|))
import Data.Sequence qualified as S
import Data.Set qualified as Set


parse :: String -> Seq (Int, Int)
parse s = S.fromList $ concat $ zipWith go [1..] cs 
  where
    go ix c
      | length c == 1 = [(ix, read [head c])]
      | otherwise = [(ix, read [head c]),(0, read [c!!1])]
    cs = chunksOf 2 s


rearrange1 :: Seq (Int, Int) -> Seq (Int, Int)
rearrange1 S.Empty = S.Empty
rearrange1 (start@(startix,startn) S.:<| others)
  | S.null others = S.singleton (startix,startn)
  | endix == 0 = rearrange1 (start <| middle) |> end
  | startix /= 0 = start <| rearrange1 (middle |> end)
  | startn == endn = (endix, endn) <| rearrange1 middle
  | startn > endn = (endix, endn) <| rearrange1 ((startix, startn - endn) <| middle)
  | startn < endn = (endix, startn) <| rearrange1 (middle |> (endix, endn-startn))
  | otherwise = error "Unforseen event in rearrange1"
  where
    (middle S.:|> end@(endix, endn)) = others


rearrange2' :: Set.Set Int -> Seq (Int, Int) -> Seq (Int, Int)
rearrange2' _ S.Empty = S.empty
rearrange2' done (others S.:|> end@(endix, endn))
  | endix `elem` done = rearrange2' done others |> end
  | null others = S.singleton end
  | startix /= 0 = start <| rearrange2' done (middle |> end)
  | endix == 0 = rearrange2' done (start <| middle) |> end
  | endn==startn = (endix, endn) <| (rearrange2' (endix `Set.insert` done) middle |> (0, startn))
  | endn < startn = (endix,endn) <| (rearrange2' (endix `Set.insert` done) ((0, startn-endn) <| middle) |> (startix, endn))
  | endn > startn = rearrange2' (endix `Set.insert` done) $ (0, startn) <| sub middle end
  | otherwise = error "Unexpected call to rearrange2"
  where
    start@(startix, startn) S.:<| middle = others
    
    sub S.Empty e = S.singleton e
    sub os e@(eix, en)
      | six/=0 = s <| sub m e
      | en==sn = ((eix,en) <| m) |> (six, sn)
      | en<sn = ((eix,en) <| ((0, sn-en) <| m)) |> (six, en)
      | en>sn = (0, sn) <| sub m e
      | otherwise = error "Unexpected call to sub"
      where
        s@(six, sn) S.:<| m = os


rearrange2 :: Set.Set Int -> Seq (Int, Int) -> Seq (Int, Int)
rearrange2 _ S.Empty = S.empty
rearrange2 done (others S.:|> end@(endix, endn))
  | endix `elem` done = rearrange2 done others |> end
  | null others = S.singleton end
  | startix /= 0 = start <| rearrange2 done (middle |> end)
  | endix == 0 = rearrange2 done (start <| middle) |> end
  | endn==startn = rearrange2 (endix `Set.insert` done) ((endix,endn) <| middle) |> (0,startn)
  | endn<startn = rearrange2 (endix `Set.insert` done) ((endix,endn) <| ((0, startn-endn) <| middle)) |> (startix,startn - endn)
  | endn>startn = rearrange2 (endix `Set.insert` done) $ (0, startn) <| sub middle end
  | otherwise = error "Unexpected call to rearrange2"
  where
    start@(startix, startn) S.:<| middle = others
    
    sub S.Empty e = S.singleton e
    sub os e@(eix, en)
      | six/=0 = s <| sub m e
      | en==sn = ((eix,en) <| m) |> (six, sn)
      | en<sn = ((eix,en) <| ((0, sn-en) <| m)) |> (six, en)
      | en>sn = (0, sn) <| sub m e
      | otherwise = error "Unexpected call to sub"
      where
        s@(six, sn) S.:<| m = os


score :: [Int] -> Int
score = sum . zipWith (\p ix -> if ix == 0 then 0 else p*(ix-1)) [0..]


expand :: Seq (Int, Int) -> [Int]
expand S.Empty = []
expand ((ix,n) S.:<| xs) = replicate n ix ++ expand xs


day9 :: IO ()
day9 = do
  ss <- getF lines 9
  --let ss = test
  let fs :: Seq (Int, Int)
      fs = parse $ head ss

  --putStrLn $ "Day9: part2: " ++ show (score $ rearrange1 $ expand1 files spaces)
  --putStrLn $ "Day9: part2: " ++ show (score . expand $ rearrange1 fs)
  --putStrLn $ "Day9: part2: " ++ show (score . expand $ rearrange2 Set.empty fs)
  putStrLn $ "Day9: part2: " ++ show (score . expand $ rearrange2' Set.empty fs)

  return ()
