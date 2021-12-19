module Day18 where

import Helpers
import Data.Char

data SnailfishNumber = Leaf Int | Pair SnailfishNumber SnailfishNumber deriving (Show, Eq)

parseSnailfishNumber :: String -> (String, SnailfishNumber)
parseSnailfishNumber ('[' : rest) = let
    (',': rest', a) = parseSnailfishNumber rest 
    (']': rest'', b) = parseSnailfishNumber rest'
    in (rest'', Pair a b)
parseSnailfishNumber x = readInt "" x
    where readInt :: String -> String -> (String, SnailfishNumber)
          readInt accum (x:rest) | isDigit x = readInt (accum ++ [x]) rest
          readInt accum x = (x, Leaf $ read accum)

parseDay18 :: String -> [SnailfishNumber]
parseDay18 = map (snd . parseSnailfishNumber) . lines

traverseWithDepth :: SnailfishNumber -> [(Int, Int)]
traverseWithDepth = inner (-1)
    where inner :: Int -> SnailfishNumber -> [(Int, Int)]
          inner depth (Leaf x) = [(depth, x)]
          inner depth (Pair l r) = inner (depth + 1) l ++ inner (depth + 1) r 

prettyPrintSnailfish :: SnailfishNumber -> String
prettyPrintSnailfish (Pair l r) = "[" ++ (prettyPrintSnailfish l) ++ "," ++ (prettyPrintSnailfish r) ++ "]"
prettyPrintSnailfish (Leaf x) = show x

data SnailfishExplodeState = Unexploded | Exploded | ExplodedL Int | ExplodedR Int | ExplodedLR Int Int deriving Show
isExploded :: SnailfishExplodeState -> Bool
isExploded Unexploded = False
isExploded _ = True

reduceSnailfish :: SnailfishNumber -> SnailfishNumber
reduceSnailfish num = let (explodeState, explodedTree) = explodeOnce 0 num
                          explodedTree' = if isExploded explodeState then
                              reduceSnailfish explodedTree
                          else
                              explodedTree

                          (splitTree, didSplit) = splitOnce explodedTree'

                          splitTree' = if didSplit then
                              reduceSnailfish splitTree
                          else
                              splitTree

                      in splitTree'
                          
    where explodeOnce :: Int -> SnailfishNumber -> (SnailfishExplodeState, SnailfishNumber)
          explodeOnce depth (Leaf x) = (Unexploded, Leaf x)
          explodeOnce 4 (Pair (Leaf l) (Leaf r)) = (ExplodedLR l r, Leaf 0)
          explodeOnce depth (Pair l r) = let
              (explodedl, l') = explodeOnce (depth + 1) l
              (explodedr, r') = explodeOnce (depth + 1) r
            in case (explodedl, explodedr) of
                (Exploded, _) -> (Exploded, Pair l' r)
                (ExplodedL lside, _) -> (ExplodedL lside, Pair l' r)
                (ExplodedR rside, _) -> (Exploded, Pair l' (addToLeftmost rside r)) 
                (ExplodedLR lside rside, _) -> (ExplodedL lside, Pair l' (addToLeftmost rside r)) 
                (Unexploded, Unexploded) -> (Unexploded, Pair l' r')
                (Unexploded, Exploded) -> (Exploded, Pair l r')
                (Unexploded, ExplodedR rside) -> (ExplodedR rside, Pair l r')
                (Unexploded, ExplodedL lside) -> (Exploded, Pair (addToRightmost lside l) r') 
                (Unexploded, ExplodedLR lside rside) -> (ExplodedR rside, Pair (addToRightmost lside l) r') 

          addToLeftmost :: Int -> SnailfishNumber -> SnailfishNumber
          addToLeftmost n (Leaf x) = Leaf (x + n) 
          addToLeftmost n (Pair l r) = Pair (addToLeftmost n l) r

          addToRightmost :: Int -> SnailfishNumber -> SnailfishNumber
          addToRightmost n (Leaf x) = Leaf (x + n) 
          addToRightmost n (Pair l r) = Pair l (addToRightmost n r)

          splitOnce :: SnailfishNumber -> (SnailfishNumber, Bool)
          splitOnce (Leaf x) | x >= 10 = let (half, mod) = x `divMod` 2 in (Pair (Leaf half) (Leaf (half + mod)), True)
          splitOnce (Leaf x) = (Leaf x, False)
          splitOnce (Pair l r) = let (l', ldid) = splitOnce l
                                     (r', rdid) = splitOnce r
                                 in (Pair l' (if ldid then r else r'), ldid || rdid)

magnitudeSnailfish :: SnailfishNumber -> Int
magnitudeSnailfish (Leaf x) = x
magnitudeSnailfish (Pair l r) = 3 * (magnitudeSnailfish l) + 2 * (magnitudeSnailfish r)

addSnailfish :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
addSnailfish l r = reduceSnailfish (Pair l r)

solveDay18Part1 :: [SnailfishNumber] -> Int
solveDay18Part1 = magnitudeSnailfish . foldl1 addSnailfish . map reduceSnailfish

solveDay18Part2 :: [SnailfishNumber] -> Int
solveDay18Part2 nums = maximum [magnitudeSnailfish $ addSnailfish l r | l <- nums, r <- nums]
