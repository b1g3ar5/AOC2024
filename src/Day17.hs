{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day17(day17) where

import Utils
import Data.Bits


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
ins (0, op) (rs@(a,b,c), p, output) = ((a `div` 2 ^ combo op rs, b, c), p+2, output) --adv
ins (1, op) (rs@(a,b,c), p, output) = ((a, b `xor` op, c), p+2, output) --bxl
ins (2, op) (rs@(a,b,c), p, output) = ((a, combo op rs `mod` 8, c), p+2, output) --bst
ins (3, op) (rs@(a,b,c), p, output) = ((a, b, c), if a /= 0 then op else p+2, output) -- jnz
ins (4, op) (rs@(a,b,c), p, output) = ((a, b `xor` c, c), p+2, output) -- bxc
ins (5, op) (rs@(a,b,c), p, output) = ((a,b,c), p+2, output ++ [combo op rs `mod` 8]) --out
ins (6, op) (rs@(a,b,c), p, output) = ((a, a `div` 2 ^ combo op rs, c), p+2, output) --bdv
ins (7, op) (rs@(a,b,c), p, output) = ((a,b, a `div` 2 ^ combo op rs), p+2, output) --cdv
ins _ _ = error "Instruction code is >7"


run :: Program -> (Regs, Pointer, Output) -> Output
run ps c@(rs, pix, output)
  | n == 0 = output
  | (pix `div` 2) >= n = output
  | otherwise = run ps $ ins (ps !! (pix `div` 2)) c
  where
    n = length ps


find :: Output -> Program -> Reg
find target prog = go (length target) 0 0
  where
    go n base x
      | n == 0 = go' base x
      | drop (length y - tlen + n) y == drop n target = go (n-1) ((base + x) `shiftL` 3) 0
      | otherwise = go n base (x+1)
      where
        tlen = length target
        y = run prog ((base + x,0,0), 0, [])

    go' base x
      | y == target = base + x
      | otherwise = go' base (x+1)
      where
        y = run prog ((base + x,0,0), 0, [])


day17 :: IO ()
day17 = do
  ss <- getLines 17
  let g = parse <$> ss
      prog = parse "[2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0]"
      prog' = reverse prog
      regs :: Regs
      regs = (37283687,0,0)
      makeRegs x = (x,0,0)
      --guess = 108107566389757::Int
      ff = find [2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0] prog

  putStrLn $ "Day17: part1: " ++ show (run prog (regs, 0, []))
  putStrLn $ "Day17: part2: " ++ show ff
  putStrLn $ "Day17: part2: " ++ show (run prog (makeRegs ff, 0, []))

  return ()


