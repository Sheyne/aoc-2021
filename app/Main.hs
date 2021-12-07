module Main where

import System.IO  
import Control.Monad
import Data.List
import Data.Bits

pairwise :: [a] -> [(a, a)]
pairwise (x:rest) = zip (x:rest) rest

triplewise :: [a] -> [(a, a, a)]
triplewise (x:rest) = map (\(a, (b, c)) -> (a, b, c)) $ zip (x:rest) $ pairwise rest

parseDay1 :: String -> [ Int ]
parseDay1 x = map read (lines x)

solveDay1Part1 :: [ Int ] -> Int
solveDay1Part1 x = length (filter (\(a, b) -> a < b) $ pairwise x)

solveDay1Part2 :: [ Int ] -> Int
solveDay1Part2 x = solveDay1Part1 $ map (\(a, b, c) -> a + b + c) $ triplewise x

data Movement = Up Int | Down Int | Forward Int deriving Show

parseMovement :: String -> Movement
parseMovement x = case words x of 
                    ["up", amt] -> Up $ read amt
                    ["down", amt] -> Down $ read amt
                    ["forward", amt] -> Forward $ read amt

parseDay2 :: String -> [ Movement ]
parseDay2 = map parseMovement . lines

solveDay2Part1 :: [ Movement ] -> Int
solveDay2Part1 moves = let (x, y) = inner moves in x * y
    where inner :: [ Movement ] -> (Int, Int)
          inner = foldr applyMove (0, 0)

          applyMove :: Movement -> (Int, Int) -> (Int, Int)
          applyMove (Up dy) (x, y) = (x, y - dy)
          applyMove (Down dy) (x, y) = (x, y + dy)
          applyMove (Forward dx) (x, y) = (x + dx, y)

solveDay2Part2 :: [ Movement ] -> Int
solveDay2Part2 moves = let (x, y, _) = inner moves in x * y
    where inner :: [ Movement ] -> (Int, Int, Int)
          inner = foldl applyMove (0, 0, 0)

          applyMove :: (Int, Int, Int) -> Movement -> (Int, Int, Int)
          applyMove (x, y, aim) (Up daim) = (x, y, aim - daim)
          applyMove (x, y, aim) (Down daim) = (x, y, aim + daim)
          applyMove (x, y, aim) (Forward d) = (x + d, y + aim * d, aim)

parseDay3 :: String -> [[Int]]
parseDay3 = map toBits . lines
    where toBits :: String -> [Int]
          toBits = map toBit

          toBit :: Char -> Int
          toBit '0' = 0
          toBit '1' = 1


bitsToInt :: [Int] -> Int
bitsToInt = rBitsToInt . reverse
    where     
        rBitsToInt :: [Int] -> Int
        rBitsToInt (x : rest) = x + 2 * rBitsToInt rest
        rBitsToInt [] = 0

solveDay3Part1 :: [[Int]] -> Int
solveDay3Part1 nums = 
    let mostlyOnes = let
                         sums = map sum $ transpose nums
                         count = length nums
                     in map (\sum -> sum > count - sum) sums
    in (bitsToInt (map (\b -> if b then 1 else 0) mostlyOnes)) * (bitsToInt (map (\b -> if b then 0 else 1) mostlyOnes))

solveDay3Part2 :: [[Int]] -> Int
solveDay3Part2 nums = let 
                          zips = map toZipList nums
                          o2 = inner 1 zips
                          co2 = inner 0 zips
                      in (bitsToInt o2) * (bitsToInt co2)
    where toZipList :: [Int] -> ([Int], Int, [Int])
          toZipList (x:rest) = ([], x, rest)
          fromZipList :: ([Int], Int, [Int]) -> [Int]
          fromZipList ([], current, right) = current : right
          fromZipList (newCurrent:left, current, right) = fromZipList (left, newCurrent, current: right)
          advance :: ([Int], Int, [Int]) -> ([Int], Int, [Int])
          advance (left, current, newCurrent:right) = (current:left, newCurrent, right)
          currents :: [([Int], Int, [Int])] -> [Int]
          currents = map (\(_, c, _) -> c)
          select :: Int -> [([Int], Int, [Int])] -> [([Int], Int, [Int])]
          select a zips = let
                              numMatching1 = length $ filter (\c -> c == 1) (currents zips)
                              numMatching0 = (length zips) - numMatching1
                              goodBit = case (a, compare numMatching0 numMatching1) of (_, EQ) -> a
                                                                                       (0, LT) -> 0
                                                                                       (0, GT) -> 1
                                                                                       (1, GT) -> 0
                                                                                       (1, LT) -> 1
                                        
                          in filter (\(_, c, _) -> c == goodBit) zips

          inner :: Int -> [([Int], Int, [Int])] -> [Int]
          inner a x = case (select a x) of selected:[] -> fromZipList selected
                                           selected -> inner a (map advance selected)

main :: IO ()
main = do
        handle <- openFile "data/day3.txt" ReadMode
        contents <- hGetContents handle
        putStrLn $ show $ solveDay3Part2 $ parseDay3 $ contents
        hClose handle