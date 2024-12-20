{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day17(day17) where

import Utils
import Data.Bits
import Control.Monad
import Data.List (tails)


type Reg = Int
type Program = [(Int, Operand)]
type Regs = (Reg, Reg, Reg)
type Operand = Int
type Pointer = Int
type Output = [Int]


parse :: String -> Program
parse s = (\n -> (n!!0, n!!1)) <$> ns
  where
    ns = chunksOf 2 $ numbers s


combo :: Int -> Regs -> Int
combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 (a,_,_) = a
combo 5 (_,b,_) = b
combo 6 (_,_,c) = c
combo 7 _ = error "Reserved opcode"
combo _ _ = error "Opcode too high"


ins :: (Int, Operand) -> (Regs, Pointer, Output) -> (Regs, Pointer, Output)
ins (2, op) (rs@(a,_,c), p, output) = ((a, combo op rs .&. 7, c), p+2, output) --bst
ins (1, op) (rs@(a,b,c), p, output) = ((a, b `xor` op, c), p+2, output) --bxl
ins (7, op) (rs@(a,b,_), p, output) = ((a, b, a `shiftR` combo op rs), p+2, output) --cdv
ins (4, _ ) (rs@(a,b,c), p, output) = ((a, b `xor` c, c), p+2, output) -- bxc
-- 1 again
ins (0, op) (rs@(a,b,c), p, output) = ((a `shiftR` combo op rs, b, c), p+2, output) --adv
ins (5, op) (rs@(a,b,c), p, output) = ((a, b, c), p+2, output ++ [combo op rs .&. 7]) -- out
ins (3, op) (rs@(a,b,c), p, output) = ((a, b, c), if a /= 0 then op else p+2, output) -- jnz

ins (6, op) (rs@(a,_,c), p, output) = ((a, a `shiftR` combo op rs, c), p+2, output) --bdv
ins _ _ = error "Instruction code is >7"


run :: Program -> (Regs, Pointer, Output) -> Output
run ps state@(_, pix, output)
  | (pix `div` 2) >= n = output
  | otherwise = run ps $ ins (ps !! (pix `div` 2)) state
  where
    n = length ps


makeRegs :: Reg -> Regs
makeRegs x = (x,0,0)


-- Call go with the list of increasing length tails of the program output
solve :: Program -> Reg
solve prog = minimum $ go 0 (tail $ reverse $ tails $ concatMap (\(a,b) -> [a,b]) prog)
  where
    -- Solves for output one at a time
    -- Takes the previous answer and shiftsL by 3 and creates the list of possibles
    -- Finds the one that satisfies the output
    -- Loops to the next slighly longer output list
    go :: Reg -> [Output] -> [Reg]
    go a [] = [a]
    go a (xs:xss) = [z | a' <- [a*8 .. a*8+7], run prog (makeRegs a', 0, []) == xs, z <- go a' xss] --do


day17 :: IO ()
day17 = do
  ss <- getLines 17
  let g = parse <$> ss
      prog = parse "[2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0]"
      target = [2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0]
      regs :: Regs
      regs = (37283687,0,0)
      ff = find target prog
      p = ins (3,0) . ins (5,5) . ins (0,3) . ins (1,3) . ins (4,1) . ins (7,5) . ins (1,3) . ins (2,4)
      xs = [0::Int,0,24,4,5,1,3,0,1,0,4,5,2,2,4,0,324861]
      ps = foldl (\acc x -> acc `shiftL` 3 + x) 0 xs

  putStrLn $ "Day17: part1: " ++ show (run prog (regs, 0, [])) -- [1,5,3,0,2,5,2,5,3]
  putStrLn $ "Day17: part2: " ++ show (solve prog) -- 108107566389757

  return ()


--------- CODE GRAVEYARD ----------------

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

-- See that the output just depends on the last 3 bits of a
-- So we can solve for each of the outputs digits in turn and
-- left shift the result etc. until we run out of digits
-- This can probably be done analytically by reversing the algorithm
find :: Output -> Program -> Reg
find target prog = go 0 0 0
  where
    l = length target
    go n base x
      | n == l = go' base x
      | lastN n y == lastN n target = go (n+1) ((base + x) `shiftL` 3) 0
      | otherwise = go n base (x+1)
      where
        y = run prog ((base + x,0,0), 0, [])

    go' base x
      | y == target = base + x
      | otherwise = go' base (x+1)
      where
        y = run prog ((base + x,0,0), 0, [])
