module Day5 where

import Helpers
import Data.List
import Data.Char

type Point = (Int, Int)

parseDay5 :: String -> [(Point, Point)]
parseDay5 x = map parseLine (lines x)
    where parseLine :: String -> (Point, Point)
          parseLine x = case map read (filter ("" /=) (splitOnF (not . isDigit) x)) of [a, b, c, d] -> ((a, b), (c, d))
                                                                                       _ -> error "invalid input"

makeGrid :: Int -> Int -> [[Int]]
makeGrid width height = replicate height (replicate width 0)
getMax :: (Point -> Int) -> [(Point, Point)] -> Int
getMax f = maximum . map f . concatMap (\(a, b) -> [a, b])

solveDay5Part1 :: [(Point, Point)] -> Int
solveDay5Part1 lines = length $ filter (>1) $ concat $ foldl (flip drawLine) (makeGrid (1+getMax fst lines) (1+getMax snd lines)) lines
    where drawRow :: Int -> Int -> [Int] -> [Int]
          drawRow 0 0 (a:r) = a+1:r
          drawRow 0 x2 (a:r) = a + 1 : drawRow 0 (x2-1) r
          drawRow x1 x2 (a:r) = a : drawRow (x1-1) (x2-1) r
          drawRow _ _ [] = []
          drawLine :: (Point, Point) -> [[Int]] -> [[Int]]
          drawLine ((x, y1), (x', y2)) grid | x == x' = transpose $ drawLine ((y1, x), (y2, x)) $ transpose grid
          drawLine ((x1, 0), (x2, 0)) (f:rest) = drawRow (min x1 x2) (max x1 x2) f : rest
          drawLine ((x1, y), (x2, y')) (f:rest) | y == y' = f : drawLine ((x1, y-1), (x2, y-1)) rest
          drawLine _ grid = grid


solveDay5Part2 :: [(Point, Point)] -> Int
solveDay5Part2 lines = length $ filter (>1) $ concat $ foldl (flip drawLine) (makeGrid (1+getMax fst lines) (1+getMax snd lines)) lines
    where drawLine :: (Point, Point) -> [[Int]] -> [[Int]]
          drawLine coords grid = foldl' drawPoint grid (linePoints coords)

          drawPoint :: [[Int]] -> Point -> [[Int]]
          drawPoint grid (x, y) = let (before, toModify: after ) = splitAt y grid
                                      (beforex, toModifyx: afterx) = splitAt x toModify
                                  in before ++ (beforex ++ (toModifyx + 1) : afterx ) : after

          dir :: Int -> Int -> Int
          dir a b | a < b = 1
          dir a b | a > b = -1
          dir _ _ = 0

          linePoints :: (Point, Point) -> [Point]
          linePoints ((x1, y1), (x2, y2)) | x1 == x2 && y1 == y2 = [(x1, y1)]
          linePoints ((x1, y1), c2@(x2, y2)) = (x1, y1) : linePoints ((x1 + dir x1 x2, y1 + dir y1 y2), c2)

drawGrid :: [[Int]] -> String
drawGrid = unlines . map (unwords . map show)
