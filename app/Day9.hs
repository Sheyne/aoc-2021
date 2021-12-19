module Day9 where

import Helpers
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Arrow

parseDay9 :: String -> [[Int]]
parseDay9 = map (map digitToInt) . lines

elementWise :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
elementWise f = zipWith (zipWith f)

convolveSame :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
convolveSame f xs = zipWith3 f (Nothing : map Just xs) xs (map Just (tail xs) ++ [Nothing])

solveDay9Part1 :: [[Int]] -> Int
solveDay9Part1 x = sum $ map ((+1) . snd) $ filter fst $ concat $ elementWise (,) (findLocalMinsSquare x) x
    where findLocalMins :: (Ord a) => [a] -> [Bool]
          findLocalMins = map (\(l, c, r) -> c < l && c < r) . triplewise

          findLocalMinsWithEnds :: (Ord a, Bounded a) => [a] -> [Bool]
          findLocalMinsWithEnds x = findLocalMins (maxBound : x ++ [maxBound])

          findLocalMinsSquare :: (Ord a, Bounded a) => [[a]] -> [[Bool]]
          findLocalMinsSquare x = elementWise (&&) (map findLocalMinsWithEnds x) (transpose (map findLocalMinsWithEnds (transpose x)))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf count list = take count list : chunksOf count (drop count list)

converge :: (Eq a) => (a -> a) -> a -> a
converge f x = if f x == x then x else converge f (f x)

solveDay9Part2 :: [[Int]] -> Int
solveDay9Part2 = map (map (/= 9))
                 >>> findConnectedComponents
                 >>> concat
                 >>> catMaybes
                 >>> countOccurances
                 >>> map fst
                 >>> sort
                 >>> reverse
                 >>> take 3
                 >>> product
    where findConnectedComponents :: [[Bool]] -> [[Maybe Int]]
          findConnectedComponents x = let seed = elementWise (\b c -> if b then Just c else Nothing) x (initialColoring (length (head x)) (length x))
                                      in converge combineNeighbors1 seed

          combineNeighbors1H :: (Ord a) => [Maybe a] -> [Maybe a]
          combineNeighbors1H = convolveSame merge'
              where merge' :: (Ord a) => Maybe (Maybe a) -> Maybe a -> Maybe (Maybe a) -> Maybe a
                    merge' a b c = merge (join a) b (join c)
                    merge :: (Ord a) => Maybe a -> Maybe a -> Maybe a -> Maybe a
                    merge (Just l) (Just m) (Just r) = Just $ min l $ min m r
                    merge Nothing (Just m) (Just r) = Just $ min m r
                    merge (Just l) (Just m) Nothing = Just $ min l m
                    merge _ x _ = x

          combineNeighbors1 :: [[Maybe Int]] -> [[Maybe Int]]
          combineNeighbors1 = transpose . map (converge combineNeighbors1H) . transpose . map (converge combineNeighbors1H)

          initialColoring :: Int -> Int -> [[Int]]
          initialColoring width height = chunksOf width [1..(width * height)]
