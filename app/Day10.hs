module Day10 where

import Helpers
import Data.List

parseDay10 :: String -> [String]
parseDay10 = lines

isOpening :: Char -> Bool
isOpening '(' = True
isOpening '{' = True
isOpening '[' = True
isOpening '<' = True
isOpening _ = False

matching :: Char -> Char
matching '(' = ')'
matching '{' = '}'
matching '<' = '>'
matching '[' = ']'
matching x = error ("Not a valid opening: '" ++ (x : "'"))

solveDay10Part1 :: [String] -> Int
solveDay10Part1 = sum . map scoreLine
    where scoreLine :: String -> Int
          scoreLine x = case parseLine x of Right _ -> 0
                                            Left ')' -> 3
                                            Left ']' -> 57
                                            Left '}' -> 1197
                                            Left '>' -> 25137
                                            Left _ -> error "invalid close"

          findClosing :: Char -> String -> Either Char String
          findClosing opener (x : whatever) | isOpening x = case parseLine (x:whatever) of Left x -> Left x
                                                                                           Right x -> findClosing opener x
          findClosing opener (x : whatever) | matching opener == x = Right whatever
          findClosing opener (x : whatever) = Left x
          findClosing opener [] = Right []

          parseLine :: String -> Either Char String
          parseLine [] = Right ""
          parseLine (opener : rest) | isOpening opener = findClosing opener rest
          parseLine _ = error "invalid line"

solveDay10Part2 :: [String] -> Int
solveDay10Part2 x = let scores = sort (filter (/=0) (map scoreLine x))
                    in scores !! (length scores `div` 2)
    where scoreLine :: String -> Int
          scoreLine = foldl (\acc x -> acc * 5 + points x) 0 . parseLine ""

          points ')' = 1
          points ']' = 2
          points '}' = 3
          points '>' = 4
          points _ = error "no points!"

          parseLine :: String -> String -> String
          parseLine stack (next:rest) | isOpening next = parseLine (next:stack) rest
          parseLine (top:stack) (next:rest) | matching top == next = parseLine stack rest
          parseLine stack "" = map matching stack
          parseLine _ _ = ""
