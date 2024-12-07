{-# LANGUAGE TupleSections #-}

module Day3_RE(day3_RE) where

import Utils hiding (string, many)
import Utils qualified as U
import Data.Char
import Data.Monoid
import Control.Applicative
import Data.Functor
import Text.Regex.Applicative
import Data.Scientific (Scientific)


test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    
pint :: RE Char Int
pint = read <$> some (psym isNumber)


pmul :: RE Char Int
pmul = combine <$> string "mul(" <*> pint <*> sym ',' <*> pint <*> sym ')'
  where
    combine :: String -> Int -> Char -> Int -> Char -> Int
    combine _ x _ y _ = x*y


parse1 :: [Char] -> [Int]
parse1 = unfoldr (((\(_, x, leftover) -> (x, leftover)) <$>) . findFirstInfix pmul )


day3_RE :: IO ()
day3_RE = do
  ss <- getF id 3

  putStrLn $ "Day3_RE: " ++ show (sum $ parse1 test)
  putStrLn $ "Day3_RE: " ++ show (sum $ parse1 ss)

  return ()

