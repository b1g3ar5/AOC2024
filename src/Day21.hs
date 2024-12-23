{-# Language BlockArguments, ImportQualifiedPost #-}

module Day21 (day21) where

import Utils ( Coord(..), lt, rt, up, dn )
import Data.MemoTrie
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S


codes :: [String]
codes = [
    "869A"
  , "170A"
  , "319A"
  , "349A"
  , "489A"]

test :: [String]
test = ["029A"
  , "980A"
  , "179A"
  , "456A"
  , "379A"]


type Pad = (Set Coord, Map Char Coord)


-- | The 4-direction pad centered on the @A@ button.
robotPad :: Pad
robotPad = (S.fromList $ M.elems mp, mp)
  where
    mp = M.fromList [                ('^', (-1, 0)), ('A', (0, 0)), 
                     ('<', (-2, 1)), ('v', (-1, 1)), ('>', (0, 1))]


doorPad :: Pad
doorPad = (S.fromList $ M.elems mp, mp)
  where
    mp = M.fromList [('7', (-2, -3)), ('8', (-1, -3)), ('9',  (0, -3))
                   , ('4', (-2, -2)), ('5', (-1, -2)), ('6', ( 0, -2))
                   , ('1', (-2, -1)), ('2', (-1, -1)), ('3', ( 0, -1))
                   ,                  ('0', (-1,  0)), ('A', ( 0,  0))]


openDoor :: String -> [String]
openDoor str =
   [ keys
   | keys <- concat <$> traverse movesToKeys (moves doorPad str)
   , isOK doorPad keys
   ]


-- Memoise level and string to shortest length
-- This will save a lot of time because this function is likely be called 
-- a lot of times. 
runRobot :: Int -> String -> Int
runRobot = memo2 \n str ->
  if n == 0 then length str else
  minimum
    [ sum (map (runRobot (n-1)) keys)
    | keys <- traverse movesToKeys (moves robotPad str)
    , isOK robotPad (concat keys)
    ]


-- Check for moves that are valid
isOK :: Pad -> [Char] -> Bool
isOK (s, _) str = all (`S.member` s) positions
  where
    positions = scanl move (0,0) str
    move here 'A' = here
    move here '>' = here + rt
    move here '<' = here + lt
    move here '^' = here + up
    move here 'v' = here + dn
    move _ _ = undefined


moves :: Pad -> String -> [Coord]
moves (_, mp) str = zipWith (-) positions ((0,0):positions)
  where
    positions = (mp M.!) <$> str


movesToKeys :: Coord -> [String]
movesToKeys (x, y) =
  [ keys ++ "A"
  | let rawKeys =
          (if y < 0 then (replicate (-y) '^' ++) else id) $
          (if x > 0 then (replicate x '>' ++) else id) $
          (if y > 0 then (replicate y 'v' ++) else id) $
          (if x < 0 then replicate (-x) '<' else "")
  , keys <- rawKeys : [reverse rawKeys | y /= 0, x /= 0]
  ]


solve :: Int -> String -> Int
solve n str = minimum (map (runRobot n) (openDoor str))


day21 :: IO ()
day21 = do
  let score n x = read (init x) * solve n x

  putStrLn $ "Day21: part1: " ++ show (sum (score 2 <$> codes))
  putStrLn $ "Day21: part2: " ++ show (sum (score 25 <$> codes))

  return ()

