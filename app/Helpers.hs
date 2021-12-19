module Helpers where

import Data.Array

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x:rest) = zip (x:rest) rest

triplewise :: [a] -> [(a, a, a)]
triplewise [] = []
triplewise (x:rest) = zipWith (curry (\(a, (b, c)) -> (a, b, c))) (x:rest) (pairwise rest)

splitOnF :: (a -> Bool) -> [a] -> [[a]]
splitOnF _ [] = []
splitOnF f s = case break f s of (x, []) -> [x]
                                 (x, _:rest) -> x : splitOnF f rest

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a = splitOnF (a ==)

applyNTimes :: Int -> (a->a) -> a -> a
applyNTimes 0 _ = id
applyNTimes n f = applyNTimes (n-1) f . f

countOccurances :: (Eq a) => [a] -> [(Int, a)]
countOccurances [] = []
countOccurances (x:rest) = increment x (countOccurances rest)
    where increment :: (Eq a) => a -> [(Int, a)] -> [(Int, a)]
          increment x [] = [(1, x)]
          increment x ((n, y):rest) | x == y = (n+1, x) : rest
          increment x (fst:rest) = fst : increment x rest

arrayFrom2dList :: [[a]] -> Array (Int, Int) a
arrayFrom2dList x = array ((0, 0), (length x - 1, length (head x) - 1)) (indexed2d x)

indexed2d :: [[a]] -> [((Int, Int), a)]
indexed2d = concatMap (\(r, row) -> map (\(c, e) -> ((r, c), e)) $ indexed row) . indexed

indexed :: [a] -> [(Int, a)]
indexed = helper 0
    where helper :: Int -> [a] -> [(Int, a)]
          helper idx [] = []
          helper idx (a:rest) = (idx, a) : helper (idx + 1) rest

bitsToInt :: [Int] -> Int
bitsToInt = rBitsToInt . reverse
    where
        rBitsToInt :: [Int] -> Int
        rBitsToInt (x : rest) = x + 2 * rBitsToInt rest
        rBitsToInt [] = 0
