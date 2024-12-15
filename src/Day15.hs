module Day15(day15) where

import Utils
import Data.Map (Map)
import Data.Map qualified as M

data Cell = BoxL | BoxR | Box | Wall | Start | Space deriving (Eq)

instance Show Cell where
  show c = [showCell c]

showCell BoxL = '['
showCell BoxR = ']'
showCell Wall = '#'
showCell Space = '.'
showCell Box = 'O'
showCell Start = '@'


pCell1 :: Char -> Cell
pCell1 '#' = Wall
pCell1 'O' = Box
pCell1 '@' = Start
pCell1 _ = Space

pCell2 :: Char -> (Cell, Cell)
pCell2 '#' = (Wall, Wall)
pCell2 'O' = (BoxL, BoxR)
pCell2 '@' = (Start, Space)
pCell2 '.' = (Space, Space)
pCell2 _ = error "Unknown cell contents"


pDir :: Char -> Direction
pDir '>' = RT
pDir '<' = LF
pDir '^' = UP
pDir 'v' = DN
pDir c = error $ "Only 4 directions: " ++ [c] ++ "?"


rows, cols1, cols2 :: Int
rows = 50 -- 10
cols1 = rows
cols2 = 2*rows

render :: Map Coord Cell -> String
render mp = unlines $ (\y -> (\x -> showCell $ mp M.! (x,y)) <$> [0.. cols2-1]) <$> [0.. rows-1]


parse1 :: [String] -> (Map Coord Cell, [Direction])
parse1 ss = (M.fromList $ parseGridWith pCell1 (ps!!0),  pDir <$> concat (ps!!1))
  where
    ps = splitOn "" ss


parseGridWith2 :: (Char -> (a,a)) -> [String] -> [(Coord, a)]
parseGridWith2  p css = concat sym
  where
    sym = concatMap (\(y, cs) -> (\(x, c) -> go x y c) <$> zip [0..] cs) $ zip [0..] css
    go x y c = [((2*x,y), a), ((2*x+1, y), b)]
      where
        (a,b) = p c


parse2 :: [String] -> (Map Coord Cell, [Direction])
parse2 ss = (M.fromList $ parseGridWith2 pCell2 (ps!!0),  pDir <$> concat (ps!!1))
  where
    ps = splitOn "" ss


move1 :: (Coord , Map Coord Cell) -> Direction -> (Coord , Map Coord Cell)
move1 (pos, g) move
  | nextCell == Wall = (pos, g)
  | nextCell == Space = (pos + toCoord move, M.insert pos Space $ M.insert (pos + toCoord move) thisCell g)
  | nextCell == Start = (pos + toCoord move, M.insert pos Space $ M.insert (pos + toCoord move) thisCell g)
  | nextCell == Box && nextCellStuck = (pos, g)
  | nextCell == Box = move1 (pos, newGrid) move
  | otherwise = error "Unknown cell contents"
  where
    thisCell = g M.! pos
    nextCell = g M.! (pos + toCoord move)
    (newNextPos, newGrid) = move1 (pos + toCoord move, g) move
    nextCellStuck = newNextPos == pos + toCoord move


boxMove2 :: (Coord , Map Coord Cell) -> Direction -> (Map Coord Cell, Bool)
boxMove2 (box, g) RT
  | nextCell == Wall = (g, False)
  | nextCell == Space = (M.insert box Space $ M.insert (box + toCoord RT) thisCell g, True)
  | nextCell == Start = (M.insert box Space $ M.insert (box + toCoord RT) thisCell g, True)
  | nextCell == BoxL && not boxMoved = (g, False)
  | nextCell == BoxL = boxMove2 (box, newGrid) RT
  | nextCell == BoxR && not boxMoved = (g, False)
  | nextCell == BoxR = boxMove2 (box, newGrid) RT
  | otherwise = error "Unknown cell contents"
  where
    thisCell = g M.! box
    nextCell = g M.! (box + toCoord RT)
    (newGrid, boxMoved) = boxMove2 (box + toCoord RT, g) RT
    

boxMove2 (box, g) LF
  | nextCell == Wall = (g, False)
  | nextCell == Space = (M.insert box Space $ M.insert (box + toCoord LF) thisCell g, True)
  | nextCell == Start = (M.insert box Space $ M.insert (box + toCoord LF) thisCell g, True)
  | nextCell == BoxR && not boxMoved = (g, False)
  | nextCell == BoxR = boxMove2 (box, newGrid) LF
  | nextCell == BoxL && not boxMoved = (g, False)
  | nextCell == BoxL = boxMove2 (box, newGrid) LF
  | otherwise = error $ "Unknown cell contents: " ++ show nextCell ++ " in: " ++ show (box + toCoord LF)
  where
    thisCell = g M.! box
    nextCell = g M.! (box + toCoord LF)
    (newGrid, boxMoved) = boxMove2 (box + toCoord LF, g) LF
-- UP and DN
boxMove2 (box, g) move
  | nextCell == Wall = (g, False)
  | nextCell == Space = (M.insert box Space $ M.insert (box + toCoord move) thisCell g, True)
  | nextCell == Start = (M.insert box Space $ M.insert (box + toCoord move) thisCell g, True)
  | nextCell == BoxL && not boxMoved = (g, False)
  | nextCell == BoxL && not boxToRMoved = (g, False)
  | nextCell == BoxL = boxMove2 (box, newGridR) move
  | nextCell == BoxR && not boxMoved = (g, False)
  | nextCell == BoxR && not boxToLMoved = (g, False)
  | nextCell == BoxR = boxMove2 (box, newGridL) move
  | otherwise = error$ "Unknown cell contents: " ++ show nextCell ++ " in: " ++ show (box + toCoord move)
  where
    thisCell = g M.! box
    nextCell = g M.! (box + toCoord move)
    (newGrid, boxMoved) = boxMove2 (box + toCoord move, g) move
    (newGridL, boxToLMoved) = boxMove2 (box + toCoord move + lt, newGrid) move
    (newGridR, boxToRMoved) = boxMove2 (box + toCoord move + rt, newGrid) move


robotMove2 :: (Coord , Map Coord Cell) -> Direction -> (Coord , Map Coord Cell)
robotMove2 (pos, g) RT
  | c == Space = (pos + toCoord RT, g)
  | c == Start = (pos + toCoord RT, g)
  | c == Wall = (pos, g)
  | c == BoxL && not boxMoved = (pos, g)
  | c == BoxL = robotMove2 (pos, newGrid) RT
  | otherwise = error "Unknown cell contents"
  where
    c = g M.! (pos + toCoord RT)
    (newGrid, boxMoved) = boxMove2 (pos + toCoord RT, g) RT
robotMove2 (pos, g) LF
  | c == Space = (pos + toCoord LF, g)
  | c == Start = (pos + toCoord LF, g)
  | c == Wall = (pos, g)
  | c == BoxR && not boxMoved = (pos, g)
  | c == BoxR = robotMove2 (pos, newGrid) LF
  | otherwise = error "Unknown cell contents"
  where
    c = g M.! (pos + toCoord LF)
    (newGrid, boxMoved) = boxMove2 (pos + toCoord LF, g) LF
-- UP or DN
robotMove2 (pos, g) move
  | nextCell == Space = (pos + toCoord move, g)
  | nextCell == Start = (pos + toCoord move, g)
  | nextCell == Wall = (pos, g)
  | nextCell == BoxL && not boxMoved = (pos, g)
  | nextCell == BoxL && not boxToRMoved = (pos, g)
  | nextCell == BoxL = robotMove2 (pos, newGridR) move
  | nextCell == BoxR && not boxMoved = (pos, g)
  | nextCell == BoxR && not boxToLMoved = (pos, g)
  | nextCell == BoxR = robotMove2 (pos, newGridL) move
  | otherwise = error "Unknown cell contents"
  where
    thisCell = g M.! pos
    nextCell = g M.! (pos + toCoord move)
    (newGrid, boxMoved) = boxMove2 (pos + toCoord move, g) move
    (newGridL, boxToLMoved) = boxMove2 (pos + toCoord move + lt, newGrid) move
    (newGridR, boxToRMoved) = boxMove2 (pos + toCoord move + rt, newGrid) move

{-
robotMove2 (pos, g) LF
  | c == Space = (pos + toCoord LF, g)
  | c == Start = (pos + toCoord LF, g)
  | c == Wall = (pos, g)
  | c == BoxR && not boxMoved = (pos, g)
  | c == BoxR = robotMove2 (pos, newGrid) LF
  | otherwise = error "Unknown cell contents"
-}
move2 :: (Coord, Map Coord Cell) -> Direction -> (Coord, Map Coord Cell)
move2 (pos, g) move
  | nextCell == Space = (pos + toCoord move, M.insert pos Space $ M.insert (pos + toCoord move) thisCell g)
  | nextCell == Start = (pos + toCoord move, M.insert pos Space $ M.insert (pos + toCoord move) thisCell g)
  | nextCell == Wall = (pos, g)
  | nextCell == BoxL && nextCellStuck = (pos, g)
  | nextCell == BoxL && (move `elem` [UP, DN]) && nextRStuck = (pos, g)
  | nextCell == BoxL = move2 (pos, newGridR) move
  | nextCell == BoxR && nextCellStuck = (pos, g)
  | nextCell == BoxR && (move `elem` [UP, DN]) && nextLStuck = (pos, g)
  | nextCell == BoxR = move2 (pos, newGridL) move
  | otherwise = error "Unknown cell contents"
  where
    thisCell = g M.! pos -- left
    nextCell = g M.! (pos + toCoord move) -- boxR 
    (newNextPos, newGrid) = move2 (pos + toCoord move, g) move -- always called
    (newL, newGridL) = move2 (pos + toCoord move + lt, newGrid) move
    (newR, newGridR) = move2 (pos + toCoord move + rt, newGrid) move -- this is called
    nextCellStuck = newNextPos == pos + toCoord move
    nextLStuck = newL == pos + toCoord move + lt
    nextRStuck = newR == pos + toCoord move + rt

score :: Cell -> (Coord, Map Coord Cell) -> Int
score xx yy = sum $ (\(x,y) -> x+100*y) . fst <$> M.toList (M.filter (==xx) $ snd yy)


day15 :: IO ()
day15 = do
  ss <- getF lines 15
  let (g1, ms) = parse1 ss
      (g2, _) = parse2 ss
      start1 = fst $ head $ M.toList $ M.filter (== Start) g1
      start2 = fst $ head $ M.toList $ M.filter (== Start) g2

  putStrLn $ "Day15: part1: " ++ show (score Box $ foldl move1 (start1, g1) ms)
  putStrLn $ "Day15: part2: " ++ show (score BoxL $ foldl robotMove2 (start2, g2) ms)

  let n = 1
  putStrLn $ "Day15: part2: " ++ show (score BoxL $ foldl robotMove2 (start2, g2) $ take n ms)
  putStrLn $ "Day15: part2: " ++ show (score BoxL $ foldl move2 (start2, g2) $ take n ms)

  putStrLn $ "Day15: part2:\n " ++ render (snd $ robotMove2 (start2, g2) $ head ms)
  putStrLn $ "Day15: part2:\n " ++ render (snd $ move2 (start2, g2) $ head ms)

  return ()
