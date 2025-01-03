module Day24(day24) where

import Utils hiding (Tree, TreeF, NodeF)
import Data.Map (Map)
import Data.Map qualified as M
--import Data.Set (Set)
--import Data.Set qualified as S 
import Data.Bits
import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )


type Signal = String
data Op = And' | Or' | Xor' deriving (Eq, Show)
type Calc = (Signal, Op, Signal)
type SigMap = Map Signal Int
type Gates = Map Signal Calc
data TreeF a r = Leaf a | Node (String, Op) r r deriving (Eq, Show, Functor)


apply :: Op -> Int -> Int -> Int
apply And' x y = x .&. y
apply Or' x y = x .|. y
apply Xor' x y = x `xor` y


-- If signal is in the signal map - it has a value, so it's a leaf
-- If it's in the gate map - it's a node
coalg :: (Signal, SigMap, Gates) -> TreeF (Int, Int) (Signal, SigMap, Gates)
coalg (sigName, sm, gates)
  | head sigName=='x' = Leaf (0, sm M.! sigName)
  | head sigName=='y' = Leaf (0, sm M.! sigName)
  | otherwise = Node (sigName, op) (xsig, sm, gates) (ysig, sm, gates)
  where
    (xsig, op, ysig) = gates M.! sigName


valueAlg :: TreeF (Int, Int) Int -> Int
valueAlg (Leaf (_,x)) = x
valueAlg (Node (_, op) l r) = apply op l r


dependents :: Signal -> Gates -> [String]
dependents z gs = nub $ hylo alg coalg z
  where
    alg :: TreeF String [String] -> [String]
    alg (Leaf s) = [s | head s /= 'x' , head s /= 'y']
    alg (Node (s, _) l r) = s : (l ++ r)

    coalg :: Signal -> TreeF String Signal
    coalg sigName
      | head sigName=='x' = Leaf sigName
      | head sigName=='y' = Leaf sigName
      | otherwise = Node (sigName, op) xsig ysig
      where
        (xsig, op, ysig) = gs M.! sigName


parseGate :: String -> (Signal, Calc)
parseGate s
  | (ws!!1) == "AND" = (ws!!4, (ws!!0, And', ws!!2))
  | (ws!!1) == "OR" = (ws!!4, (ws!!0, Or', ws!!2))
  | (ws!!1) == "XOR" = (ws!!4, (ws!!0, Xor', ws!!2))
  | otherwise = error $ "No parse of: " ++ s
  where
    ws = words s


parse :: [String] -> (SigMap, Gates)
parse s = (wireMap, M.fromList gates)
  where
    ps = splitOn "" s
    wireMap = M.fromList $ (\w -> (init (w!!0), read $ w!!1)) . words <$> (ps!!0)
    gates = parseGate <$> ps!!1


getSig :: Signal -> SigMap -> Int
getSig s sm = fromJust $ M.lookup s sm


toInt :: [Int] -> Int
toInt = foldl (\acc z -> 2*acc + z) 0 . reverse


asBinary45 :: Int -> String
asBinary45 x = replicate (45 - length s) '0' ++ s
  where
    s = showIntAtBase 2 intToDigit x ""


makeWires :: Int -> Int -> SigMap
makeWires x y = M.fromList $ xs ++ ys
  where
    xs, ys :: [(Signal, Int)]
    xs = zipWith (\i d -> (toX i, read [d]))  [0..44] $ reverse $ asBinary45 x
    ys = zipWith (\i d -> (toY i, read [d]))  [0..44] $ reverse $ asBinary45 y
    toX i = 'x':(if i<10 then "0" else []) ++ show i
    toY i = 'y':(if i<10 then "0" else []) ++ show i


add :: Gates -> Int -> Int -> Int
add gates x y = toInt $ go <$> [0..45]
  where
    go :: Int -> Int
    go n = hylo valueAlg coalg (zsig, makeWires x y, gates)
      where
        zsig = toSig 'z' n
    

findError :: Gates -> Int
findError gates = go 1
  where
    go :: Int -> Int
    go x
      | z == (x+y) = go (x*2)
      | otherwise = x
      where
        y=x
        z = add gates x y


toSig :: Char -> Int -> Signal
toSig z n = [z] ++ (if n<10 then "0" else "") ++ show n


getCrucialGates :: Gates -> Int -> [Signal]
getCrucialGates gs n = nub $ ret1 ++ ret2
  where
    xs = toSig 'x' n
    ys = toSig 'y' n
    ret1 = M.keys $ M.filterWithKey (\k (x, _, y) -> k == toSig 'z' n || x `elem` [xs, ys] || y `elem` [xs, ys]) gs
    ret2 = M.keys $ M.filterWithKey (\_ (x, _, y) -> x `elem` ret1 || y `elem` ret1) gs


nextFail :: Gates -> Int
nextFail gates = round (logBase 2 $ fromIntegral $ findError gates)


swapable :: (Signal, Signal) -> Gates -> Bool
swapable (x,y) gs = x `notElem` ydeps && y `notElem` xdeps
  where
    xdeps = dependents x gs
    ydeps = dependents y gs


swapGates :: (Signal, Signal) -> Gates -> Gates
swapGates (x, y) gs = M.insert x yv $ M.insert y xv gs
  where
    !xv = gs M.! x
    !yv = gs M.! y


fixGates :: Gates -> [Signal]
fixGates startGates = go (nextFail startGates) [] startGates
  where
    go :: Int -> [Signal] -> Gates -> [Signal]
    go n acc gs
      | n == 45 = acc
      | otherwise = go nf (s1:s2:acc) newGates
      where
        crucial = getCrucialGates gs (if n == 9 then n else n+1)
        pairs = [(g,h) | g <- crucial, h <- crucial, g < h, swapable (g,h) gs]
        (nf, (s1,s2), newGates) = ggo pairs gs
        ggo :: [(Signal, Signal)] -> Gates -> (Int, (Signal, Signal), Gates)
        ggo [] _ = error "None of the pairs worked"
        ggo (p:ps) ggs
          | nff > (n+1) = (nff, p, ngs)
          | otherwise = ggo ps ggs
          where
            ngs = swapGates p ggs
            nff = nextFail ngs


day24 :: IO ()
day24 = do
  ss <- getF lines 24
  let (initialMap, gates) = parse ss
      allz = toSig 'z' <$> [0..45::Int]

  putStrLn $ "Day24: part1: " ++ show (toInt $ (\z -> hylo valueAlg coalg (z, initialMap, gates)) <$> allz) 
  putStrLn $ "Day24: gates: " ++ intercalate "," (sort $ fixGates gates) 
  --putStrLn $ "Day24: part2: " ++ intercalate "," (sort ["qwf", "cnk", "vhm", "z14", "z27", "mps", "msq", "z39"])
  return ()

