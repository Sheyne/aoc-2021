module Day8 where

import Helpers
import Data.List


parseDay8 :: String -> [([[Char]], [[Char]])]
parseDay8 = map (\line -> case map (filter (/= "") . words) $ splitOn '|' line of [a, b] -> (a, b)
                                                                                  _ -> error "bad line") . lines

solveDay8Part1 :: [([[Char]], [[Char]])] -> Int
solveDay8Part1 = sum . map inner
    where inner :: ([[Char]], [[Char]]) -> Int
          inner (_, output) = length $ filter (`elem` easyLengths) $ map length output
          easyLengths = [2, 4, 3, 7]

solveDay8Part2 :: [([[Char]], [[Char]])] -> Int
solveDay8Part2 = sum . map decode
    where
          decode :: ([[Char]], [[Char]]) -> Int
          decode (observed, message) = let mapping = solveMapping observed
                                           digits = getDigits mapping message
                                       in sum $ zipWith (\digit idx -> digit * (10 ^ idx)) (reverse digits) [0..]
          getDigits :: [(String, Int)] -> [String] -> [Int]
          getDigits mapping = map (getDigit mapping)
          getDigit :: [(String, Int)] -> String -> Int
          getDigit mapping key = snd $ head $ filter (\(k, _) -> sort k == sort key) mapping

          solveMapping :: [[Char]] -> [(String, Int)]
          solveMapping x = let fromEasies = foldl' intersectOption unitDigit (map easyToOptions (selectEasies x))
                               maps = allMappings (flip notElem) fromEasies
                           in head $ map fst $ filter (uncurry isConsistant) [(coding, map) | map <- maps, coding <- getAllCodings x]

          anything = "abcdefg"
          easyLengths = [2, 4, 3, 7]
          unitDigit = [anything, anything, anything, anything, anything, anything, anything]
          selectEasies :: [String] -> [String]
          selectEasies = filter ((`elem` easyLengths) . length)

          easyToOptions :: String -> [String]
          easyToOptions x = makeNumOptions x (easyToDigit (length x))

          makeNumOptions :: String -> Int -> [String]
          makeNumOptions x digit = map (boolToOptions x) (makeNumImage digit)
              where boolToOptions :: String -> Bool -> String
                    boolToOptions x True = x
                    boolToOptions x False = anything \\ x

          easyToDigit :: Int -> Int
          easyToDigit x = case lengthToDigits x of [d] -> d
                                                   _ -> error "Not an easy one"

          makeNumImage :: Int -> [Bool]
          makeNumImage 0 = [True, True, True, False, True, True, True]
          makeNumImage 1 = [False, False, True, False, False, True, False]
          makeNumImage 2 = [True, False, True, True, True, False, True]
          makeNumImage 3 = [True, False, True, True, False, True, True]
          makeNumImage 4 = [False, True, True, True, False, True, False]
          makeNumImage 5 = [True, True, False, True, False, True, True]
          makeNumImage 6 = [True, True, False, True, True, True, True]
          makeNumImage 7 = [True, False, True, False, False, True, False]
          makeNumImage 8 = [True, True, True, True, True, True, True]
          makeNumImage 9 = [True, True, True, True, False, True, True]
          makeNumImage _ = error "not a valid digit"

          lengthToDigits :: Int -> [Int]
          lengthToDigits 2 = [1]
          lengthToDigits 3 = [7]
          lengthToDigits 4 = [4]
          lengthToDigits 5 = [2, 3, 5]
          lengthToDigits 6 = [0, 6, 9]
          lengthToDigits 7 = [8]
          lengthToDigits _ = error "Not a valid length"

          getAllCodings :: [String] -> [[(String, Int)]]
          getAllCodings = allMappings (\x (a, b) -> a `notElem` map fst x && b `notElem` map snd x) . map (\code -> (\digits -> [(code, digit) | digit <- digits]) $ lengthToDigits $ length code)

          intersectOption :: [[Char]] -> [[Char]] -> [[Char]]
          intersectOption = zipWith intersect

          lightUp :: String -> [Char] -> [Bool]
          lightUp signal mapping = map (`elem` signal) mapping

          isConsistant :: [(String, Int)] -> [Char] -> Bool
          isConsistant patterns mapping = all (testPattern mapping) patterns
              where testPattern :: [Char] -> (String, Int) -> Bool
                    testPattern mapping (key, digit) = lightUp key mapping == makeNumImage digit

          allMappings :: ([a] -> a -> Bool) -> [[a]] -> [[a]]
          allMappings = inner []
              where inner :: [a] -> ([a] -> a -> Bool) -> [[a]] -> [[a]]
                    inner _ _ [] = [[]]
                    inner taken selector (options:rest) = [x:suffix | x <- filter (selector taken) options, suffix <- inner (x:taken) selector rest]
