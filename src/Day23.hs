{-# LANGUAGE LambdaCase #-}

module Day23(day23) where

import Utils
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)


parse :: String -> (String, String)
parse s = (ps!!0, ps!!1)
  where
    ps = splitOn '-' s


type Node = String
type Graph = Map Node [Node]
type Triple = Set Node
type Clique = Set Node


makeMap :: [(Node, Node)] -> Graph
makeMap = go M.empty
  where
    go :: Graph -> [(Node, Node)] -> Graph
    go mp [] = mp
    go mp ((from, to):others) = go (M.insertWith (++) to [from] $ M.insertWith (++) from [to] mp) others


groups :: Graph -> Set Triple
groups mp = S.unions $ S.fromList . findSet <$> M.keys mp
  where
    findSet :: String -> [Triple]
    findSet k = [ S.fromList [k,c,d] | c <- ks, d <- ks, c /= d, d `elem` mp M.! c]
      where
        ks = mp M.! k


hasT :: Set Node -> Bool
hasT s = any (\e -> 't' == head e) (S.toList s)


-- Double counting, probably
solve :: Graph -> Set Clique
solve g = makeCliques (S.fromList $ M.keys g) S.empty
  where
    makeCliques :: Set Node -> Set Clique -> Set Clique
    makeCliques nodes cliques
      | S.null nodes = cliques 
      | otherwise = makeCliques remaining (clique `S.insert` cliques)
      where
        mv = S.minView nodes
        (node, otherNodes) = fromJust mv
        clique = go (S.singleton node)(S.singleton node) (S.fromList $ g M.! node) 
        remaining = otherNodes S.\\ clique
    
    -- Make a clique from a node
    go :: Set Node -> Clique -> Set Node -> Clique
    go seen clique queue
      | isNothing mv = clique
      | next `S.member` seen = go seen clique others
      | next `S.member` clique = go seen clique others
      | isNextConnected = go (next `S.insert` seen) (next `S.insert` clique) others 
      | otherwise = go (next `S.insert` seen) clique others 
      where
        mv = S.minView queue
        (next, others) = fromJust mv
        isNextConnected = and $ S.map (\c -> next `elem` g M.! c) clique


day23 :: IO ()
day23 = do
  ss <- getF lines 23
  let g = parse <$> ss
      mp = makeMap g

  putStrLn $ "Day23: part1: " ++ show (S.size $ S.filter hasT $ groups mp)
  putStrLn $ "Day23: part2: " ++  intercalate "," (sort $ S.toList $ last $ sortOn length $ S.toList $ solve mp)
  
  return ()


