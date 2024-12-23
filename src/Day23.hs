{-# LANGUAGE LambdaCase #-}

module Day23(day23) where

import Utils
import Data.List (mapAccumL)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
--import Data.Algorithm.MaximalCliques
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import qualified Data.Vector as V


parse :: String -> (String, String)
parse s = (ps!!0, ps!!1)
  where
    ps = splitOn '-' s


type Conns = Map String [String]
type Triple = Set String


makeMap :: [(String, String)] -> Conns
makeMap = go M.empty
  where
    go :: Map String [String] -> [(String, String)] -> Map String [String]
    go mp [] = mp
    go mp ((from, to):others) = go (M.insertWith (++) to [from] $ M.insertWith (++) from [to] mp) others


groups :: Conns -> Set Triple
--groups mp = S.fromList $ concatMap findSet $ M.keys mp
groups mp = S.unions $ S.fromList . findSet <$> M.keys mp
  where
    findSet :: String -> [Triple]
    findSet k = [ S.fromList [k,c,d] | c <- ks, d <- ks, c /= d, d `elem` mp M.! c]
      where
        ks = mp M.! k


hast :: Set String -> Bool
hast s = any (\e -> 't' == head e) (S.toList s)


-- Bron-Kerbosch algorithm - copied from Data.Algorithm.MaximalCliques
getMaximalCliques :: (a -> a -> Bool) -> [a] -> [[a]]
getMaximalCliques tolFun xs = ((fst . (V.!) lv) <$>) . I.toList <$>
                              maximalCliques pickpivot (snd . (V.!) lv) (I.fromList $ map fst lnodes)
  where 
    lnodes = zip [0..] xs
    lnodes' = (\(k,n) -> (n, I.fromList $ filter (/=k) $ map fst $ filter (tolFun n . snd) lnodes)) <$> lnodes
    lv = V.fromList lnodes'
    pickpivot p x = head $ I.elems p ++ I.elems x


maximalCliques :: (IntSet -> IntSet -> Int) -> (Int -> IntSet) -> IntSet -> [IntSet]
maximalCliques pickpivot neighborsOf nodeset = go I.empty nodeset I.empty
  where 
    go r p x
      | I.null p && I.null x = [r]
      | otherwise = concat . snd $ mapAccumL step' (p,x) $ I.elems (p I.\\ neighborsOf pivot)
      where
        pivot = pickpivot p x
        step' (p',x') v = ((I.delete v p', I.insert v x'), go (I.insert v r) (I.intersection nv p') (I.intersection nv x'))
          where
            nv  = neighborsOf v


day23 :: IO ()
day23 = do
  ss <- getF lines 23
  let g = parse <$> ss
      mp = makeMap g
      cs = getMaximalCliques (\a b -> a `elem` mp M.! b) $ M.keys mp


  putStrLn $ "Day23: part1: " ++ show (S.size $ S.filter hast $ groups mp)
  putStrLn $ "Day23: part2: " ++  intercalate "," (sort $ last $ sortOn length cs)
  
  return ()
