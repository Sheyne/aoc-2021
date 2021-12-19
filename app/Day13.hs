module Day13 where

import Helpers
import Control.Monad.ST
import Control.Monad.Loops
import Data.Array.ST
import Data.Array.IArray
import Data.STRef
import Data.IORef (modifyIORef)
import Data.List


parseDay13 :: String -> ([(Int, Int)], [(Char, Int)])
parseDay13 x = let [points, folds] = splitOn [] $ lines x
               in (map (\q -> let [a, b] = splitOn ',' q in (read a, read b)) points,
                  map (\q -> let [[a], b] = splitOn '=' $ drop 11 q in (a, read b)) folds)

pad :: [[Bool]] -> Int -> [[Bool]]
pad arr len = replicate (len - length arr) (replicate (length (head arr)) False) ++ arr

foldPaper :: Int -> [[Bool]] -> [[Bool]]
foldPaper l m = let a = take l m
                    b = drop (l + 1) m
                    largerLen = max (length a) (length b)
                    a' = pad a largerLen
                    b' = pad (reverse b) largerLen
                 in zipWith (zipWith (||)) a' b'

foldPaper' :: Bool -> Int -> [[Bool]] -> [[Bool]]
foldPaper' False x = foldPaper x
foldPaper' True x = transpose . foldPaper x . transpose

gridFromPoints :: [(Int, Int)] -> [[Bool]]
gridFromPoints points = runST $ do
    let width = maximum $ map fst points
    let height = maximum $ map snd points
    arr <- newArray ((0, 0), (height, width)) False  :: ST s (STArray s (Int, Int) Bool)

    mapM_ (\(x, y) -> writeArray arr (y, x) True) points

    sequence [do
            row <- mapIndices ((y, 0), (y, width)) id arr
            getElems row | y <- [0..height]]

solveDay13Part1 :: ([(Int, Int)], [(Char, Int)]) -> Int
solveDay13Part1 (points, folds) = let grid = gridFromPoints points
                                      folded = foldl (\grid fold-> foldPaper' ('x' == fst fold) (snd fold) grid) grid [head folds]
                                  in length $ filter id $ concat folded

solveDay13Part2 :: ([(Int, Int)], [(Char, Int)]) -> String
solveDay13Part2 (points, folds) = let grid = gridFromPoints points
                                      folded = foldl (\grid fold-> foldPaper' ('x' == fst fold) (snd fold) grid) grid folds
                                  in unlines $ map (map (\x -> if x then '#' else '.')) folded
