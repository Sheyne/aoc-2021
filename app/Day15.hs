module Day15 where

import Helpers
import qualified Data.Heap as H
import qualified Data.Map as Map
import Data.List
import Data.Array
import Data.Maybe
import Data.Char

parseDay15 :: String -> [[Int]]
parseDay15 = map (map digitToInt) . lines

astar :: (Eq l, Ord l, Show l) => (l -> [(Int, l)]) -> (l -> Int) -> l -> Int
astar neighbors heuristic startingLoc = astar' neighbors heuristic (Map.insert startingLoc 0 Map.empty) (H.insert (heuristic startingLoc, startingLoc) H.empty)
    where astar' :: (Eq l, Ord l, Show l) => (l -> [(Int, l)]) -> (l -> Int) -> Map.Map l Int -> H.MinPrioHeap Int l -> Int
          astar' neighbors heuristic bestKnown paths = let
                  ((_, loc), otherPaths) = fromJust $ H.view paths
                  score = fromJust $ Map.lookup loc bestKnown
                  neighborhood = filter (\(c, l) -> maybe True (>(c + score)) (Map.lookup l bestKnown)) $ neighbors loc
                  newPaths = map (\(cost, dest) -> (cost + score, dest)) neighborhood
                  newPathsH = (H.fromList $ map (\(c, p) -> (c + heuristic p, p)) newPaths)
              in
                  if heuristic loc == 0 then score else
                  astar' neighbors heuristic (foldr (\(v, k) bn -> Map.insert k v bn) bestKnown newPaths) (H.union newPathsH otherPaths)

solveDay15Part1 :: [[Int]] -> Int
solveDay15Part1 board =
    let arrayBoard = arrayFrom2dList board
        ((_, _), (height, width)) = bounds arrayBoard
        lookup = (arrayBoard !)
    in astar (neighbors lookup width height) (heuristic width height) (0, 0)
    where neighbors :: ((Int, Int)->Int) -> Int -> Int -> (Int, Int) -> [(Int, (Int, Int))]
          neighbors lookup width height (x, y) = map (\l -> (lookup l, l)) $
                filter (validBoardLoc width height) [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]

          validBoardLoc :: Int -> Int -> (Int, Int) -> Bool
          validBoardLoc width height (x, y) = x >= 0 && x <= width && y >= 0 && y <= height

          heuristic :: Int -> Int -> (Int, Int) -> Int
          heuristic width height (x, y) = width - x + height - y

solveDay15Part2 :: [[Int]] -> Int
solveDay15Part2 = solveDay15Part1 . expand
    where expand :: [[Int]] -> [[Int]]
          expand grid = concat [map (\row -> concat [map (\c -> ((c + x + y - 1) `rem` 9) + 1) row | x <- [0..4]]) grid
                                | y <- [0..4]]
