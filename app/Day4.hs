module Day4 where

import Helpers
import Data.Maybe
import Data.List

data Board = Board { draws :: [Int], boards :: [[[Int]]] } deriving Show

parseDay4 :: String -> Board
parseDay4 x = let
                  [first] : boards = splitOn "" $ splitOn '\n' x
              in Board { draws = map read (splitOn ',' first) , boards = map parseBoard boards }
              where parseBoard :: [String] -> [[Int]]
                    parseBoard = map parseRow
                    parseRow :: String -> [Int]
                    parseRow = map read . filter ("" /=) . words


prefixes :: Int -> [a] -> [[a]]
prefixes n x = take n x : prefixes (n + 1) x
markBoard :: [Int] -> [[Int]] -> [[Bool]]
markBoard selected = map $ map $ flip elem selected
checkBoard :: [[Bool]] -> Bool
checkBoard x = checkRows x || checkRows (transpose x)
checkRows :: [[Bool]] -> Bool
checkRows = any and
scoreBoard :: [Int] -> [[Int]] -> Int
scoreBoard called board = last called * sum (filter (not . flip elem called) $ concat board)

solveDay4Part1 :: Board -> Int
solveDay4Part1 b = head $ mapMaybe (solveForPrefix (boards b)) (prefixes 0 (draws b))
    where solveForPrefix :: [[[Int]]] -> [Int] -> Maybe Int
          solveForPrefix boards called = let
                                             boardCompletions = map (checkBoard . markBoard called) boards
                                             finishedBoards = map fst (filter snd (zip boards boardCompletions))
                                         in case finishedBoards of [] -> Nothing
                                                                   board: _ -> Just $ scoreBoard called board


solveDay4Part2 :: Board -> Int
solveDay4Part2 b = step (boards b) (replicate (length (boards b)) False) (prefixes 0 (draws b))
    where step :: [[[Int]]] -> [Bool] -> [[Int]] -> Int
          step boards previous (called:nextToCall) = let
                                                         boardCompletions = map (checkBoard . markBoard called) boards
                                                     in if and boardCompletions then
                                                            let
                                                                lastBoard = head (map snd (filter (not . fst) (zip previous boards)))
                                                            in scoreBoard called lastBoard
                                                        else step boards boardCompletions nextToCall
          step _ _ _ = error "must have calls left"
