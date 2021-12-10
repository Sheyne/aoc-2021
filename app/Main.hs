module Main where

import System.IO
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Text.ParserCombinators.ReadP (count)
import Control.Arrow
import Debug.Trace

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x:rest) = zip (x:rest) rest

triplewise :: [a] -> [(a, a, a)]
triplewise [] = []
triplewise (x:rest) = zipWith (curry (\(a, (b, c)) -> (a, b, c))) (x:rest) (pairwise rest)

parseDay1 :: String -> [ Int ]
parseDay1 x = map read (lines x)

solveDay1Part1 :: [ Int ] -> Int
solveDay1Part1 x = length (filter (uncurry (<)) $ pairwise x)

solveDay1Part2 :: [ Int ] -> Int
solveDay1Part2 x = solveDay1Part1 $ map (\(a, b, c) -> a + b + c) $ triplewise x

data Movement = Up Int | Down Int | Forward Int deriving Show

parseMovement :: String -> Movement
parseMovement x = case words x of
                    ["up", amt] -> Up $ read amt
                    ["down", amt] -> Down $ read amt
                    ["forward", amt] -> Forward $ read amt
                    _ -> error "invalid movement"

parseDay2 :: String -> [ Movement ]
parseDay2 = map parseMovement . lines

solveDay2Part1 :: [ Movement ] -> Int
solveDay2Part1 moves = let (x, y) = inner moves in x * y
    where inner :: [ Movement ] -> (Int, Int)
          inner = foldr applyMove (0, 0)

          applyMove :: Movement -> (Int, Int) -> (Int, Int)
          applyMove (Up dy) (x, y) = (x, y - dy)
          applyMove (Down dy) (x, y) = (x, y + dy)
          applyMove (Forward dx) (x, y) = (x + dx, y)

solveDay2Part2 :: [ Movement ] -> Int
solveDay2Part2 moves = let (x, y, _) = inner moves in x * y
    where inner :: [ Movement ] -> (Int, Int, Int)
          inner = foldl applyMove (0, 0, 0)

          applyMove :: (Int, Int, Int) -> Movement -> (Int, Int, Int)
          applyMove (x, y, aim) (Up daim) = (x, y, aim - daim)
          applyMove (x, y, aim) (Down daim) = (x, y, aim + daim)
          applyMove (x, y, aim) (Forward d) = (x + d, y + aim * d, aim)

parseDay3 :: String -> [[Int]]
parseDay3 = map toBits . lines
    where toBits :: String -> [Int]
          toBits = map toBit

          toBit :: Char -> Int
          toBit '0' = 0
          toBit '1' = 1
          toBit _ = error "invalid bit"


bitsToInt :: [Int] -> Int
bitsToInt = rBitsToInt . reverse
    where
        rBitsToInt :: [Int] -> Int
        rBitsToInt (x : rest) = x + 2 * rBitsToInt rest
        rBitsToInt [] = 0

solveDay3Part1 :: [[Int]] -> Int
solveDay3Part1 nums =
    let mostlyOnes = let
                         sums = map sum $ transpose nums
                         count = length nums
                     in map (\sum -> sum > count - sum) sums
    in bitsToInt (map (\b -> if b then 1 else 0) mostlyOnes) * bitsToInt (map (\b -> if b then 0 else 1) mostlyOnes)

solveDay3Part2 :: [[Int]] -> Int
solveDay3Part2 nums = let
                          zips = map toZipList nums
                          o2 = inner 1 zips
                          co2 = inner 0 zips
                      in bitsToInt o2 * bitsToInt co2
    where toZipList :: [Int] -> ([Int], Int, [Int])
          toZipList (x:rest) = ([], x, rest)
          toZipList [] = error "zip list must be non-empty"
          fromZipList :: ([Int], Int, [Int]) -> [Int]
          fromZipList ([], current, right) = current : right
          fromZipList (newCurrent:left, current, right) = fromZipList (left, newCurrent, current: right)
          advance :: ([Int], Int, [Int]) -> ([Int], Int, [Int])
          advance (left, current, newCurrent:right) = (current:left, newCurrent, right)
          advance _ = error "advance must have right available"
          currents :: [([Int], Int, [Int])] -> [Int]
          currents = map (\(_, c, _) -> c)
          select :: Int -> [([Int], Int, [Int])] -> [([Int], Int, [Int])]
          select a zips = let
                              numMatching1 = length $ filter (== 1) (currents zips)
                              numMatching0 = length zips - numMatching1
                              goodBit = case (a, compare numMatching0 numMatching1) of (_, EQ) -> a
                                                                                       (0, LT) -> 0
                                                                                       (0, GT) -> 1
                                                                                       (1, GT) -> 0
                                                                                       (1, LT) -> 1
                                                                                       _ -> error "a must be zero or 1"

                          in filter (\(_, c, _) -> c == goodBit) zips

          inner :: Int -> [([Int], Int, [Int])] -> [Int]
          inner a x = case select a x of [selected] -> fromZipList selected
                                         selected -> inner a (map advance selected)

data Board = Board { draws :: [Int], boards :: [[[Int]]] } deriving Show

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a = splitOnF (a ==)

splitOnF :: (a -> Bool) -> [a] -> [[a]]
splitOnF _ [] = []
splitOnF f s = case break f s of (x, []) -> [x]
                                 (x, _:rest) -> x : splitOnF f rest

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

parseDay6 :: String -> [Int]
parseDay6 = map read . splitOn ','

applyNTimes :: Int -> (a->a) -> a -> a
applyNTimes 0 _ = id
applyNTimes n f = applyNTimes (n-1) f . f

solveDay6Parts :: Int -> [Int] -> Int
solveDay6Parts numDays = sum . run numDays . setup
    where setup :: [Int] -> [Int]
          setup = foldl' (flip (addNFish 1)) []

          addNFish :: Int -> Int -> [Int] -> [Int]
          addNFish n 0 [] = [n]
          addNFish n 0 (numToday:rest) = numToday + n : rest
          addNFish n days [] = 0 : addNFish n (days - 1) []
          addNFish n days (numToday:rest) = numToday : addNFish n (days - 1) rest

          run :: Int -> [Int] -> [Int]
          run = flip applyNTimes step

          step :: [Int] -> [Int]
          step [] = []
          step (zeros:rest) = addNFish zeros 6 $ addNFish zeros 8 rest

solveDay6Part1 :: [Int] -> Int
solveDay6Part1 = solveDay6Parts 80

solveDay6Part2 :: [Int] -> Int
solveDay6Part2 = solveDay6Parts 256

parseDay7 :: String -> [Int]
parseDay7 = parseDay6

solveDay7Part1 :: [Int] -> Int
solveDay7Part1 = minimum . costs
    where costs :: [Int] -> [Int]
          costs positions = map (`cost` positions) [0..(maximum positions)]
          cost :: Int -> [Int] -> Int
          cost target = sum . map (\x -> abs (x - target))

solveDay7Part2 :: [Int] -> Int
solveDay7Part2 = minimum . costs
    where costs :: [Int] -> [Int]
          costs positions = map (`cost` positions) [0..(maximum positions)]
          cost :: Int -> [Int] -> Int
          cost target = sum . map (\x -> sum [0..abs (x - target)])

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

parseDay9 :: String -> [[Int]]
parseDay9 = map (map digitToInt) . lines

elementWise :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
elementWise f = zipWith (zipWith f)

convolveSame :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
convolveSame f xs = zipWith3 f (Nothing : map Just xs) xs (map Just (tail xs) ++ [Nothing])

solveDay9Part1 :: [[Int]] -> Int
solveDay9Part1 x = sum $ map ((+1) . snd) $ filter fst $ concat $ elementWise (,) (findLocalMinsSquare x) x
    where findLocalMins :: (Ord a) => [a] -> [Bool]
          findLocalMins = map (\(l, c, r) -> c < l && c < r) . triplewise

          findLocalMinsWithEnds :: (Ord a, Bounded a) => [a] -> [Bool]
          findLocalMinsWithEnds x = findLocalMins (maxBound : x ++ [maxBound])

          findLocalMinsSquare :: (Ord a, Bounded a) => [[a]] -> [[Bool]]
          findLocalMinsSquare x = elementWise (&&) (map findLocalMinsWithEnds x) (transpose (map findLocalMinsWithEnds (transpose x)))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf count list = take count list : chunksOf count (drop count list)

countOccurances :: (Eq a) => [a] -> [(Int, a)]
countOccurances [] = []
countOccurances (x:rest) = increment x (countOccurances rest)
    where increment :: (Eq a) => a -> [(Int, a)] -> [(Int, a)]
          increment x [] = [(1, x)]
          increment x ((n, y):rest) | x == y = (n+1, x) : rest
          increment x (fst:rest) = fst : increment x rest

converge :: (Eq a) => (a -> a) -> a -> a
converge f x = if f x == x then x else converge f (f x)

solveDay9Part2 :: [[Int]] -> Int
solveDay9Part2 = map (map (/= 9))
                 >>> findConnectedComponents
                 >>> concat
                 >>> catMaybes
                 >>> countOccurances
                 >>> map fst
                 >>> sort
                 >>> reverse
                 >>> take 3
                 >>> product
    where findConnectedComponents :: [[Bool]] -> [[Maybe Int]]
          findConnectedComponents x = let seed = elementWise (\b c -> if b then Just c else Nothing) x (initialColoring (length (head x)) (length x))
                                      in converge combineNeighbors1 seed

          combineNeighbors1H :: (Ord a) => [Maybe a] -> [Maybe a]
          combineNeighbors1H = convolveSame merge'
              where merge' :: (Ord a) => Maybe (Maybe a) -> Maybe a -> Maybe (Maybe a) -> Maybe a
                    merge' a b c = merge (join a) b (join c)
                    merge :: (Ord a) => Maybe a -> Maybe a -> Maybe a -> Maybe a
                    merge (Just l) (Just m) (Just r) = Just $ min l $ min m r
                    merge Nothing (Just m) (Just r) = Just $ min m r
                    merge (Just l) (Just m) Nothing = Just $ min l m
                    merge _ x _ = x

          combineNeighbors1 :: [[Maybe Int]] -> [[Maybe Int]]
          combineNeighbors1 = transpose . map (converge combineNeighbors1H) . transpose . map (converge combineNeighbors1H)

          initialColoring :: Int -> Int -> [[Int]]
          initialColoring width height = chunksOf width [1..(width * height)]


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

solvePuzzle = solveDay10Part2 . parseDay10

main :: IO ()
main = do
        handle <- openFile "data/day10.txt" ReadMode
        contents <- hGetContents handle
        print $ solvePuzzle contents
        hClose handle