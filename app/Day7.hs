module Day7 where

import Helpers


parseDay7 :: String -> [Int]
parseDay7 = map read . splitOn ','

solveDay7Part1 :: [Int] -> Int
solveDay7Part1 = minimum . costs
    where costs :: [Int] -> [Int]
          costs positions = map (`cost` positions) [0..(maximum positions)]
          cost :: Int -> [Int] -> Int
          cost target = sum . map (\x -> abs (x - target))

solveDay7Part2 :: [Int] -> Int
solveDay7Part2 = minimum . costs
    where costs :: [Int] -> [Int]
          costs positions = map (`cost` positions) [0..(maximum positions)]
          cost :: Int -> [Int] -> Int
          cost target = sum . map (\x -> sum [0..abs (x - target)])
