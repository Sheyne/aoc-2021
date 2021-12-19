module Day6 where

import Data.List
import Helpers

parseDay6 :: String -> [Int]
parseDay6 = map read . splitOn ','

solveDay6Parts :: Int -> [Int] -> Int
solveDay6Parts numDays = sum . run numDays . setup
    where setup :: [Int] -> [Int]
          setup = foldl' (flip (addNFish 1)) []

          addNFish :: Int -> Int -> [Int] -> [Int]
          addNFish n 0 [] = [n]
          addNFish n 0 (numToday:rest) = numToday + n : rest
          addNFish n days [] = 0 : addNFish n (days - 1) []
          addNFish n days (numToday:rest) = numToday : addNFish n (days - 1) rest

          run :: Int -> [Int] -> [Int]
          run = flip applyNTimes step

          step :: [Int] -> [Int]
          step [] = []
          step (zeros:rest) = addNFish zeros 6 $ addNFish zeros 8 rest

solveDay6Part1 :: [Int] -> Int
solveDay6Part1 = solveDay6Parts 80

solveDay6Part2 :: [Int] -> Int
solveDay6Part2 = solveDay6Parts 256
