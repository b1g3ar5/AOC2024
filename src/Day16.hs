module Day16(day16) where

import Utils
import Data.Set qualified as S
import Data.Set (Set)
import Data.PQueue.Prio.Min qualified as Q
import Data.Map (Map)
import Data.Map qualified as M


type State = (Coord, Coord) -- position and direction


parseCell :: Char -> Bool
parseCell = (=='#')


parse :: [String] -> [Coord]
parse ss = (fst <$>) $ filter snd $ parseGridWith parseCell ss


getStart, getFinish :: [String] -> Coord
getStart = fst . head . filter snd . parseGridWith (=='S')
getFinish = fst . head . filter snd . parseGridWith (=='E')


-- Returns visited which is a map with the lowest score for each state
dijkstra :: Q.MinPQueue Int State -> Map State Int -> (State -> [State]) -> (State -> State -> Int) -> (State -> Bool) -> Map State Int
dijkstra pipeline visited nextFn costFn finishFn
  | Q.null pipeline = visited
  | finishFn state = visited
  | state `M.member` visited = dijkstra remainingPipeline newVisited nextFn costFn finishFn
  | otherwise = dijkstra newPipeline newVisited nextFn costFn finishFn
  where
    ((savedMin, state), remainingPipeline) = Q.deleteFindMin pipeline
    newStates = nextFn state
    newPipeline = remainingPipeline `Q.union` Q.fromList ((\n -> (savedMin + costFn state n, n)) <$> newStates)
    newVisited = M.insertWith min state savedMin visited


solve :: (Coord, Coord) -> Set Coord -> Map State Int
solve (start, direction) walls = dijkstra (Q.fromList [(0, (start, direction))]) M.empty nextStates cost (const False)
  where
    cost :: State -> State -> Int
    cost (from, dir) (to, _)
      | to == from + dir = 1
      | otherwise = 1000

    nextStates :: State -> [State]
    nextStates (pos, dir) = [(pos, clockTurn dir), (pos, antiTurn dir)] ++ ([(pos+dir, dir) | not ((pos + dir) `S.member` walls)])


day16 :: IO ()
day16 = do
  ss <- getF lines 16
  let walls = S.fromList $ parse ss
      start = getStart ss
      finish = getFinish ss
      fwd = solve (start, rt) walls
      bwd = solve (finish, lt) walls
      minScore = fwd M.! (finish, (1,0))

      -- Combine fwd and bwd maps - reversing directions of bwd ..
      bothWays = M.unionWith (+) fwd $ M.mapKeys (\(p,d) -> (p, -d)) bwd
      
      -- Get rid of the directions and keep the minimum score for each position
      -- then filter out the ones with score == minScore
      reduced = M.filter (==minScore) $ M.mapKeysWith min fst bothWays


  putStrLn $ "Day16: part1: " ++ show minScore
  putStrLn $ "Day16: part2: " ++ show (M.size reduced)

  return ()



