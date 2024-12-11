module Day9(day9) where

import Utils
import Data.Sequence (Seq, (|>), (<|))
import Data.Sequence qualified as S
import Data.Map (Map)
import Data.Map qualified as M


-- I found part2 difficult until I thought of using maps
-- My initial rearrangeFile with sequences not maps was comlicated because
-- unlike part1 the spaces have to stay on the front in case
-- a later file is small enough to fit in.

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


score :: [Int] -> Int
score = sum . zipWith (\p ix -> if ix == 0 then 0 else p*(ix-1)) [0..]


expand :: Seq (Int, Int) -> [Int]
expand S.Empty = []
expand ((ix,n) S.:<| xs) = replicate n ix ++ expand xs


-- Rearranges one file and returns the updates maps
rearrangeFile :: (Map Int (Int, Int), [(Int, Int)]) -> Int -> (Map Int (Int, Int), [(Int, Int)])
rearrangeFile (fm, []) _ = (fm, [])
rearrangeFile (fm, (cur,len):spaces) fileIx
  | cur >= pos = (fm, (cur,len):spaces) -- No spaces nearer the start
  | len == sz = (M.insert fileIx (cur, sz) fm, spaces) -- Fills all the spaces
  | len > sz = (M.insert fileIx (cur, sz) fm, (cur+sz, len-sz):spaces) -- Takes up sz of these spaces
  | otherwise = second ((cur,len):) $ rearrangeFile (fm, spaces) fileIx -- Not enough space
  where
    (pos, sz) = fm M.! fileIx


day9 :: IO ()
day9 = do
  ss <- getF lines 9
  let fs :: Seq (Int, Int)
      fs = parse $ head ss
      fileMap :: Map Int (Int, Int)
      fileMap = snd $ foldl' (\(cur, m) (ix, sz) -> (cur + sz, if ix /=0 then M.insert ix (cur, sz) m else m)) (0, M.empty) fs
      spaceMap :: [(Int, Int)]
      spaceMap = reverse $ snd $ foldl' (\(cur, m) (ix, sz) -> (cur + sz, if ix /=0 then m else (cur, sz):m)) (0, []) fs
      (newFileMap, _) = foldl' rearrangeFile (fileMap, spaceMap) $ reverse $ M.keys fileMap

  putStrLn $ "Day9: part1: " ++ show (score . expand $ rearrange1 fs)
  putStrLn $ "Day9: part2: " ++ show (sum $ (\(ix, (cursor,size)) -> (ix-1) * (2*cursor+size-1)*size `quot` 2) <$> M.toList newFileMap)

  return ()


test = ["2333133121414131402"]