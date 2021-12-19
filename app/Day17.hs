module Day17 where

import Helpers
import qualified Data.Set as Set
import Data.Ix

parseDay17 :: String -> ((Int, Int), (Int, Int))
parseDay17 z = let [_, x, y] = splitOn '=' z
               in (inner (head (splitOn ',' x)), inner y)
    where inner :: String -> (Int, Int)
          inner x = let [a, _, b] = splitOn '.' x
                    in (read a, read b)

solveDay17Part1 :: ((Int, Int), (Int, Int)) -> Int
solveDay17Part1 (xs, ys) = let
    (xnear, xfar) = solveRange xs
    (ynear, yfar) = solveRange ys
    trajectories = [trajectory 0 0 vx vy xfar yfar | vx <- [(signum xfar)..xfar], vy <- [0..200]]
    trajectories' = filter (any (\(x, y) -> inRange xs x && inRange ys y)) trajectories
    in maximum $ map snd (concat trajectories')

    where solveRange :: (Int, Int) -> (Int, Int)
          solveRange (x1, x2) | x1 < 0 && x2 < 0 = (x2, x1)
          solveRange (x1, x2) = (x1, x2)
        
          trajectory :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
          trajectory x y _ _ xmax ymax | abs x > abs xmax || signum ymax * y > signum ymax * ymax = []
          trajectory x y vx vy xmax ymax = (x, y) : trajectory (x+vx) (y+vy) (vx - signum vx) (vy-1) xmax ymax

solveDay17Part2 :: ((Int, Int), (Int, Int)) -> Int
solveDay17Part2 (xs, ys) = let
    (xnear, xfar) = solveRange xs
    (ynear, yfar) = solveRange ys
    trajectories = [((vx, vy), trajectory 0 0 vx vy xfar yfar) | vx <- [(signum xfar)..xfar], vy <- [-200..2000]]
    trajectories' = filter (\(v, tr) -> any (\(x, y) -> inRange xs x && inRange ys y) tr) trajectories
    in length (Set.fromList (map fst trajectories'))

    where solveRange :: (Int, Int) -> (Int, Int)
          solveRange (x1, x2) | x1 < 0 && x2 < 0 = (x2, x1)
          solveRange (x1, x2) = (x1, x2)
        
          trajectory :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
          trajectory x y _ _ xmax ymax | abs x > abs xmax || signum ymax * y > signum ymax * ymax = []
          trajectory x y vx vy xmax ymax = (x, y) : trajectory (x+vx) (y+vy) (vx - signum vx) (vy-1) xmax ymax
