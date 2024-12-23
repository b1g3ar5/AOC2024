{-# LANGUAGE MultiWayIf #-}

module Day21_old(day21) where

import Utils
import Data.Set qualified as S
import Data.Map qualified as M
import Control.Monad
import Data.MemoTrie


type Pad = (S.Set Coord, M.Map Char Coord)


parse :: String -> Int
parse = read

codes = ["869A"
  , "170A"
  , "319A"
  , "349A"
  , "489A"]

test = ["029A"
  , "980A"
  , "179A"
  , "456A"
  , "379A"]

keyPad :: Pad
keyPad = (S.fromList $ M.elems mp, mp )
  where
    mp = M.fromList [('0', (1,0))
          , ('1', (0,1))
          , ('2', (1,1))
          , ('3', (2,1))
          , ('5', (1,2))
          , ('6', (2,2))
          , ('7', (0,3))
          , ('8', (1,3))
          , ('9', (2,3))
          , ('A', (2,0))]


robotPad :: Pad
robotPad = (S.fromList $ M.elems mp, mp )
  where
    mp = M.fromList [('<', (0,0))
          , ('v', (1,0))
          , ('>', (2,0))
          , ('^', (1,1))
          , ('A', (2,1))]


robotRoute :: Char -> Char -> [String]
robotRoute '<' 'v' = (++ "A") <$> [">"]
robotRoute '<' '>' = (++ "A") <$> [">>"]
robotRoute '<' '^' = (++ "A") <$> both  ">^"
robotRoute '<' 'A' = (++ "A") <$> both ">>^"
robotRoute 'v' '<' = (++ "A") <$> ["<"]
robotRoute 'v' '>' = (++ "A") <$> [">"]
robotRoute 'v' '^' = (++ "A") <$> ["^"]
robotRoute 'v' 'A' = (++ "A") <$> both ">^"
robotRoute '>' 'v' = (++ "A") <$> ["<"]
robotRoute '>' '<' = (++ "A") <$> ["<<"]
robotRoute '>' '^' = (++ "A") <$> both "<^"
robotRoute '>' 'A' = (++ "A") <$> ["^"]
robotRoute '^' '<' = (++ "A") <$> both "v<"
robotRoute '^' 'v' = (++ "A") <$> ["v"]
robotRoute '^' '>' = (++ "A") <$> ["v>"]
robotRoute '^' 'A' = (++ "A") <$> [">"]
robotRoute 'A' '<' = (++ "A") <$> both "v<<"
robotRoute 'A' 'v' = (++ "A") <$> both "v<"
robotRoute 'A' '>' = (++ "A") <$> ["v"]
robotRoute 'A' '^' = (++ "A") <$> ["<"]
robotRoute '<' '<' = (++ "A") <$> [""]
robotRoute 'v' 'v' = (++ "A") <$> [""]
robotRoute '>' '>' = (++ "A") <$> [""]
robotRoute '^' '^' = (++ "A") <$> [""]
robotRoute 'A' 'A' = (++ "A") <$> [""]
robotRoute a b = error $ "Unknown key on keyPad: " ++ [a] ++ "->" ++ [b]


both :: Eq a => [a] -> [[a]]
both xs = [xs, reverse xs]

keyRoute :: Coord -> [String]
keyRoute = memo $ \ (x,y) ->
  let go = if
            | x==0 && y==0 -> [""]
            | (y<0) && (x<0) -> both $ replicate (-x) '<' ++ replicate (-y) 'v'
            | y<0 -> both $ replicate x '>' ++ replicate (-y) 'v'
            | x<0 -> both $ replicate y '^' ++ replicate (-x) '<'
            | otherwise -> both $ replicate y '^' ++ replicate x '>'
  in nub $ (++ "A") <$> go


moveToKeys :: Coord -> [String]
moveToKeys (x,y) =
  [ keys ++ "A"
  | let rawKeys =
          (if y < 0 then (replicate (-y) '^' ++) else id) $
          (if x > 0 then (replicate x '>' ++) else id) $
          (if y > 0 then (replicate y 'v' ++) else id) $
          (if x < 0 then replicate (-x) '<' else "")
  , keys <- rawKeys : [reverse rawKeys | y /= 0, x /= 0]
  ]


routesKeyPad :: String -> [String]
routesKeyPad = memo $ \xs ->
    --let rs = zipWith (\from to -> keyRoute $ keyPad M.! to - keyPad M.! from) ('A':xs) xs
    let rs = moveToKeys <$> moves keyPad xs
    in (concat <$>) $ sequence rs


routesRobotPad :: String -> [String]
routesRobotPad = memo $ \xs ->
  let rs :: [[String]]
      rs = zipWith robotRoute ('A':xs) xs
  in concat (sequence rs)


runRobot :: Int -> String -> Int
runRobot = memo2 $ \n str ->
  if n == 0 then length str else
  minimum
    [ sum (map (runRobot (n-1)) keys)
    | let ms = moves robotPad str
    --, keys <- traverse moveToKeys ms
    , keys <- traverse keyRoute ms
    , isOK robotPad (concat keys)
    ]


solve :: Int -> String -> Int
solve n str = minimum (map (runRobot n) (doorInputs str))


answer :: Int -> String -> Int
answer n str = minimum (map (robotLength n) (doorInputs str))


padFromList :: [(Coord, Char)] -> Pad
padFromList xs = (S.fromList [p | (p, _) <- xs], M.fromList [(c,p) | (p,c) <- xs])

padCoord :: Pad -> Char -> Coord
padCoord (_, m) c = m M.! c

inPad :: Pad -> Coord -> Bool
inPad (s, _) x = S.member x s


doorInputs :: String -> [String]
doorInputs str =
   [ keys
   | let deltas = moves keyPad str
   , keys <- concat <$> traverse moveToKeys deltas
   , isOK keyPad keys
   ]

robotLength :: Int -> String -> Int
robotLength = memo2 $ \n str ->
  if n == 0 then length str else
  minimum
    [ sum (map (robotLength (n-1)) keys)
    | let deltas = moves robotPad str
    , keys <- traverse moveToKeys deltas
    , isOK robotPad (concat keys)
    ]

isOK :: Pad -> [Char] -> Bool
isOK pad str = all (inPad pad) posns
  where
    posns = scanl move (0,0) str
    move here 'A' = here
    move here '>' = here + rt 
    move here '<' = here + lt
    move here '^' = here + up
    move here 'v' = here + dn
    move _ _ = undefined

moves :: Pad -> String -> [Coord]
moves pad str = zipWith (-) positions ((0,0):positions)
  where
    positions = map (padCoord pad) str


day21 :: IO ()
day21 = do
  let score n x = read (init x) * answer n x

  putStrLn $ "Day21: part1: " ++ show (sum (score  2 <$> codes))
  putStrLn $ "Day21: part2: " ++ show (sum (score 25 <$> codes))
  --putStrLn $ "Day21: part2: " ++ show (doorInputs "379A")
  --putStrLn $ "Day21: part2: " ++ show (routesKeyPad "379A")

  return ()


