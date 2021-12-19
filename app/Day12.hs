module Day12 where

import Helpers
import Data.Char

parseDay12 :: String -> [(String, String)]
parseDay12 = map (\x -> let [a, b] = splitOn '-' x in (a, b)) . lines

solveDay12Part1 :: [(String, String)] -> Int
solveDay12Part1 x = length $ visit x [["start"]] "start"

    where neighbors :: [(String, String)] -> String -> [String]
          neighbors connections key = map fst (filter ((== key).snd) connections) ++ map snd (filter ((== key).fst) connections)

          visit :: [(String, String)] -> [[String]] -> String -> [[String]]
          visit connections previouses current = do
                previous <- previouses
                next <- filter (canFollow previous) $ neighbors connections current
                if next == "end" then
                    [next : previous]
                else
                    visit connections [ next: previous] next

          canFollow :: [String] -> String -> Bool
          canFollow _ x | isUpper $ head x = True
          canFollow previous x = x `notElem` previous

solveDay12Part2 :: [(String, String)] -> Int
solveDay12Part2 x = length $ visit x [(False, ["start"])] "start"

    where neighbors :: [(String, String)] -> String -> [String]
          neighbors connections key = map fst (filter ((== key).snd) connections) ++ map snd (filter ((== key).fst) connections)

          visit :: [(String, String)] -> [(Bool, [String])] -> String -> [[String]]
          visit connections previouses current = do
                (hasUsed, previous) <- previouses
                ((hasUsed, _), next) <- filter (snd.fst) $ map (\x -> (canFollow (hasUsed, previous) x, x)) $ neighbors connections current
                if next == "end" then
                    [next : previous]
                else
                    visit connections [(hasUsed, next: previous)] next

          canFollow :: (Bool, [String]) -> String -> (Bool, Bool)
          canFollow (hasUsed, _) x | isUpper $ head x = (hasUsed, True)
          canFollow (hasUsed, _) x | x == "start" = (hasUsed, False)
          canFollow (False, previous) x = (x `elem` previous, True)
          canFollow (True, previous) x = (True, x `notElem` previous)
