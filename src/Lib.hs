module Lib
    ( libMain
    ) where

import Utils (timeIt)
import Day1
import Day2
import Day3
import Day3_RE
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

libMain :: IO ()
libMain = do 
  day1
  day2
  day3
  --day3_RE
  day4
  day5
  timeIt day6
  day7
  day8
  day9
  day10
  day11
  day12
  day13
  day14
  day15
  day16
  day17
  day18
  day19
  day20
  day21
  timeIt day22
  day23
  day24
  day25
