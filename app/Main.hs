module Main where

import System.IO  
import Control.Monad

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

main :: IO ()
main = do  
        handle <- openFile "data/day1-input1.txt" ReadMode
        contents <- hGetContents handle
        putStrLn $ show $ solveDay1Part2 $ parseDay1 $ contents
        hClose handle   

