module Day24_old(day24) where

import Utils hiding (Tree, TreeF, NodeF)
import Data.Map (Map)
import Data.Map qualified as M
import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )
import Data.Bits


type Op = (Int -> Int -> Int)
type Name = String
type Gate = (Name, Op, Name)
type Gates  = Map Name Gate
type Wires = Map Name Int


getWire :: Name -> Wires -> Int
getWire s sm = sm M.! s


parseOp :: String -> Op
parseOp "AND" = \a b -> a .&. b
parseOp "OR" = \a b -> a .|. b
parseOp "XOR" = \a b -> a `xor` b
parseOp _ = error "Error in parseOp"


parse :: [String] -> (Wires, Gates)
parse s = (wireMap, M.fromList gates)
  where
    ps = splitOn "" s
    wireMap = M.fromList $ (\w -> (init (w!!0), read $ w!!1)) . words <$> (ps!!0)
    e = words <$> ps!!1
    -- 4 = gate name, 0 = first wire, 1 = operation 2 = second wire
    gates = (\w -> (w!!4, (w!!0, parseOp $ w!!1, w!!2))) <$> e
    in1 = (!!0) <$> e
    in2 = (!!2) <$> e
    in3 = (!!4) <$> e
    addMap = M.fromList $ (,Nothing) <$> in1 ++ in2 ++ in3



applyGate :: Wires -> (Name, Gates) -> Wires
applyGate wmp (out, gates)
  | isNothing x = undefined
  | otherwise = M.insert out v wmp
  where
    (xn, op, yn) = gates M.! out
    x = wmp M.!? xn
    y = wmp M.!? yn
    v = op (fromJust x) (fromJust y)


goUntil :: Wires -> ([Name], Gates) -> Wires
goUntil ws ([],_) = ws
goUntil ws ((n:ns), gates)
  | not (null zs) = goUntil (applyGate ws (n, gates)) (ns, gates)
  | otherwise = ws
  where
    zs = M.elems $ M.filterWithKey (\k _ -> head k == 'z') ws



getXs, getYs, getZs :: Wires -> [Int]
getXs = M.elems . M.filterWithKey (\k _ -> head k == 'x')
getYs = M.elems . M.filterWithKey (\k _ -> head k == 'y')
getZs = M.elems . M.filterWithKey (\k _ -> head k == 'z')


asBinary45 :: Int -> String
asBinary45 x = (take (45 - length s) $ repeat '0') ++ s
  where
    s = showIntAtBase 2 intToDigit x ""


makeWires :: Int -> Int -> Wires
makeWires x y = M.fromList $ xs ++ ys
  where
    xs, ys :: [(Name, Int)]
    xs = zipWith (\i d -> (toX i, read [d]))  [0..44] $ reverse $ asBinary45 x
    ys = zipWith (\i d -> (toY i, read [d]))  [0..44] $ reverse $ asBinary45 y
    toX i = 'x':(if i<10 then "0" else []) ++ show i
    toY i = 'y':(if i<10 then "0" else []) ++ show i


runGates :: Wires -> Gates -> [Name] -> Int
runGates wires gates names = foldl (\acc z -> 2*acc + z) 0 $ reverse zs
  where
    zs = getZs $ (\n -> goUntil wires (n,gates)) $ cycle names



toInt :: [Bool] -> Int
toInt = foldl (\acc z -> 2*acc + (if z then 1 else 0)) 0 . reverse


errlu mp i
  | isNothing ret = error $ "Not item: " ++ show i
  | otherwise = fromJust ret

  where
    ret = mp M.!? i


day24 :: IO ()
day24 = do
  ss <- getF lines 24
  let (wires, gates) = parse $ ss
      names = M.keys wires


  putStrLn $ "Day24: part1: " ++ show (runGates wires gates names)
  putStrLn $ "Day24: part2: " ++ show (runGates (makeWires 26495111766585 18168650014383) gates names)
  putStrLn $ "Day24: part2: " ++ show ((makeWires 26495111766585 18168650014383) == wires)

  return ()



