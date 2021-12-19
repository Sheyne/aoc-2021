module Day3 where

import Helpers
import Data.List

parseDay3 :: String -> [[Int]]
parseDay3 = map toBits . lines
    where toBits :: String -> [Int]
          toBits = map toBit

          toBit :: Char -> Int
          toBit '0' = 0
          toBit '1' = 1
          toBit _ = error "invalid bit"


solveDay3Part1 :: [[Int]] -> Int
solveDay3Part1 nums =
    let mostlyOnes = let
                         sums = map sum $ transpose nums
                         count = length nums
                     in map (\sum -> sum > count - sum) sums
    in bitsToInt (map (\b -> if b then 1 else 0) mostlyOnes) * bitsToInt (map (\b -> if b then 0 else 1) mostlyOnes)

solveDay3Part2 :: [[Int]] -> Int
solveDay3Part2 nums = let
                          zips = map toZipList nums
                          o2 = inner 1 zips
                          co2 = inner 0 zips
                      in bitsToInt o2 * bitsToInt co2
    where toZipList :: [Int] -> ([Int], Int, [Int])
          toZipList (x:rest) = ([], x, rest)
          toZipList [] = error "zip list must be non-empty"
          fromZipList :: ([Int], Int, [Int]) -> [Int]
          fromZipList ([], current, right) = current : right
          fromZipList (newCurrent:left, current, right) = fromZipList (left, newCurrent, current: right)
          advance :: ([Int], Int, [Int]) -> ([Int], Int, [Int])
          advance (left, current, newCurrent:right) = (current:left, newCurrent, right)
          advance _ = error "advance must have right available"
          currents :: [([Int], Int, [Int])] -> [Int]
          currents = map (\(_, c, _) -> c)
          select :: Int -> [([Int], Int, [Int])] -> [([Int], Int, [Int])]
          select a zips = let
                              numMatching1 = length $ filter (== 1) (currents zips)
                              numMatching0 = length zips - numMatching1
                              goodBit = case (a, compare numMatching0 numMatching1) of (_, EQ) -> a
                                                                                       (0, LT) -> 0
                                                                                       (0, GT) -> 1
                                                                                       (1, GT) -> 0
                                                                                       (1, LT) -> 1
                                                                                       _ -> error "a must be zero or 1"

                          in filter (\(_, c, _) -> c == goodBit) zips

          inner :: Int -> [([Int], Int, [Int])] -> [Int]
          inner a x = case select a x of [selected] -> fromZipList selected
                                         selected -> inner a (map advance selected)
