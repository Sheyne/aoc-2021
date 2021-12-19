{-# LANGUAGE TupleSections #-}

module Day14 where

import Helpers
import Data.List


parseDay14 :: String -> (String, [((Char, Char), Char)])
parseDay14 x = let [[initial], mappings] = splitOn [] $ lines x
               in (initial,
                  map (\q -> let [a, b, _, _, _, _, c] = q in ((a, b), c)) mappings)

stepPairInsertion :: [((Char, Char), Char)] -> String -> String
stepPairInsertion mapping x@(a:_) = a : concatMap (\p@(l, r) ->
                case find ((== p).fst) mapping of Just (_, i) -> [i, r]
                                                  Nothing -> [r]) (pairwise x)
stepPairInsertion _ a = a

solveDay14Part1 :: (String, [((Char, Char), Char)]) -> Int
solveDay14Part1 (initial, mappings) = let s = applyNTimes 10 (stepPairInsertion mappings) initial
                                          occurances = sort $ countOccurances s
                                          (leastCommon, _) = head occurances
                                          (mostCommon, _) = last occurances
                                      in mostCommon - leastCommon

joinCounts :: (Eq a) => [(Int, a)] -> [(Int, a)]
joinCounts [] = []
joinCounts (x:rest) = inner x (joinCounts rest)
    where inner :: (Eq a) => (Int, a) -> [(Int, a)] -> [(Int, a)]
          inner x [] = [x]
          inner (n1, x) ((n2, y):rest) | x == y = (n1+n2, x) : rest
          inner x (fst:rest) = fst : inner x rest


solveDay14Part2 :: (String, [((Char, Char), Char)]) -> Int
solveDay14Part2 (initial, mappings) = let
        (letterCounts, _) = applyNTimes 40 (step mappings) (countOccurances initial, countOccurances (pairwise initial))
        letterCounts' = sort letterCounts
        (leastCommon, _) = head letterCounts'
        (mostCommon, _) = last letterCounts'
        in mostCommon - leastCommon
    where pairBecomes :: [((Char, Char), Char)] -> (Char, Char) -> ([(Char, Char)], [Char])
          pairBecomes mapping pair@(l, r) = case find ((== pair).fst) mapping of
                                        Just (_, x) -> ([(l, x), (x, r)], [x])
                                        Nothing -> ([pair], [])

          step :: [((Char, Char), Char)] -> ([(Int, Char)], [(Int, (Char, Char))]) -> ([(Int, Char)], [(Int, (Char, Char))])
          step mappings (letterCounts, pairCounts) =
              let newCounts = map (\(count, pair) ->
                                let (pairs, letters) = pairBecomes mappings pair
                                in (map (count,) pairs, map (count,) letters)) pairCounts
                  newPairCounts = joinCounts (concatMap fst newCounts)
                  newLetterCounts = joinCounts (letterCounts ++ concatMap snd newCounts)
              in (newLetterCounts, newPairCounts)
