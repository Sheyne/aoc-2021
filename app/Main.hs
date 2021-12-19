module Main where

import System.IO

import Day1
import Day2
import Day3
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
import Helpers

solvePuzzle = solveDay18Part2 . parseDay18

main :: IO ()
main = do
        handle <- openFile "data/day18.txt" ReadMode
        contents <- hGetContents handle
        print $ solvePuzzle contents
        hClose handle