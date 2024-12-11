module Day11(day11) where


data Tree a = Tree (Tree a) a (Tree a)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

nats :: Tree Int
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
        where
          l = n + s
          r = l + s
          s' = s * 2

index :: Show a => Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
                         (q,0) -> index l q
                         (q,1) -> index r q
                         _ -> error "This shouldn't happen"


parse :: String -> [Int]
parse = (read <$>) .  words


type Stone = Int
type Iters = Int

decode :: Int -> (Stone, Iters)
decode si = si `quotRem` 100

encode :: (Stone, Iters) -> Int
encode (s,i) = s * 100 + i


changeStoneF :: (Int -> Int) -> Int -> Int
changeStoneF f sn
  | n == 0 = 1
  | s == 0 = f $ encode (1, n-1)
  | even len  = f (encode (leftShone, n-1))  + f (encode (rightStone, n-1))
  | otherwise = f $ encode (2024 * s, n-1)
  where
    (s,n) = decode sn
    ss = show s
    len = length ss
    leftShone = read $ take (len `quot` 2) ss
    rightStone = read $ drop (len `quot` 2) ss


stoneTree :: Tree Int
stoneTree = fmap (changeStoneF fastStone) nats

fastStone :: Int -> Int
fastStone = index stoneTree


day11 :: IO ()
day11 = do
  let s = "0 44 175060 3442 593 54398 9 8101095"
  let g = parse s

  putStrLn $ "Day11: part2: " ++ show (sum $ fastStone . encode . (, 25) <$> g)
  putStrLn $ "Day11: part2: " ++ show (sum $ fastStone . encode . (, 75) <$> g)

  return ()


