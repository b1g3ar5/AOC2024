{-# LANGUAGE QuasiQuotes #-}

module Day3(day3) where

import Utils


-- I tried without a parser with regex etc - but failed
parseMul :: ReadP Int
parseMul = do
  _ <- string "mul("
  x <- pInt
  _ <- char ','
  y <- pInt
  _ <- char ')'
  return $ x*y


-- The lazy way - just crawl along the sting a character at a time...
parse1 :: Int -> String -> Int
parse1 acc [] = acc
parse1 acc s
  | "mul(" == take 4 s = if null p then parse1 acc (drop 4 s) else parse1 (acc + n) leftover
  | otherwise = parse1 acc $ tail s
  where
    p :: [(Int, String)]
    p = parseS parseMul s -- null if parse fails
    n = fst $ head p
    leftover = snd $ head p
    

-- Similarly, crawl along, but jump on a few chars on do, dont, mul
parse2 :: Bool -> Int -> String -> Int
parse2 _ acc [] = acc
parse2 False acc s
  | "do()" == take 4 s = parse2 True acc $ drop 4 s
  | otherwise = parse2 False acc $ tail s
parse2 True acc s
  | "don't()" == take 7 s = parse2 False acc $ drop 7 s
  | "mul(" == take 4 s = if null p then parse2 True acc (drop 4 s) else parse2 True (acc + n) leftover
  | otherwise = parse2 True acc $ tail s
  where
    p :: [(Int, String)]
    p = parseS parseMul s -- null if parse fails
    n = fst $ head p
    leftover = snd $ head p


day3 :: IO ()
day3 = do
  ss <- getF id 3

  putStrLn $ "Day3: part1: " ++ show (parse1 0 ss)
  putStrLn $ "Day3: part2: " ++ show (parse2 True 0 ss)

  return ()
