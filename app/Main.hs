{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import System.IO
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Arrow
import Debug.Trace
import Control.Monad.ST
import Control.Monad.Loops
import Data.Array.ST
import Data.Array.IArray
import Data.STRef
import Data.IORef (modifyIORef)
import qualified Data.Heap as H
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bool (bool)

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

parseDay11 :: String -> [[Int]]
parseDay11 = parseDay9

indexed :: [a] -> [(Int, a)]
indexed = helper 0
    where helper :: Int -> [a] -> [(Int, a)]
          helper idx [] = []
          helper idx (a:rest) = (idx, a) : helper (idx + 1) rest

indexed2d :: [[a]] -> [((Int, Int), a)]
indexed2d = concatMap (\(r, row) -> map (\(c, e) -> ((r, c), e)) $ indexed row) . indexed

arrayFrom2dList :: [[a]] -> Array (Int, Int) a
arrayFrom2dList x = array ((0, 0), (length x - 1, length (head x) - 1)) (indexed2d x)

incrNeighbors :: STArray s (Int, Int) (Maybe Int) -> Int -> Int -> (Int, Int) -> ST s ()
incrNeighbors x height width y = do
    writeArray x y Nothing
    mapM_ (\loc -> do
        oldVal <- readArray x loc
        let newVal = fmap (+1) oldVal
        writeArray x loc newVal) (neighbors height width y)

neighbors :: Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbors h w (y, x) = [(ny, nx) | ny <- [(max 0 (y-1))..(min h (y+1))], nx <- [(max 0 (x-1))..(min w (x+1))]]

modifyAll :: STArray s (Int, Int) a -> (a -> a) -> ST s ()
modifyAll a f = do
    assocs <- getAssocs a
    mapM_ (\(loc, x) -> writeArray a loc $ f x) assocs


solveDay11Part1 :: [[Int]] -> Int
solveDay11Part1 x = runST $ do
    numBursts <- newSTRef 0
    arr <- thaw $ arrayFrom2dList (map (map Just) x) :: ST s (STArray s (Int, Int) (Maybe Int))
    ((_, _), (height, width)) <- getBounds arr

    mapM_ (\_ -> do
            modifyAll arr (fmap (+1))
            whileJust_ (listToMaybe . map fst . filter (\x -> case snd x of Just x -> x >= 10
                                                                            Nothing -> False)
                                                                            <$> getAssocs arr)
                (\x -> do incrNeighbors arr height width x
                          modifySTRef numBursts (+1))
            modifyAll arr (\case Just x -> Just x ; Nothing -> Just 0)
        ) [1..100]

    readSTRef numBursts

solveDay11Part2 :: [[Int]] -> Int
solveDay11Part2 x = runST $ do
    arr <- thaw $ arrayFrom2dList (map (map Just) x) :: ST s (STArray s (Int, Int) (Maybe Int))
    ((_, _), (height, width)) <- getBounds arr
    iteration <- newSTRef 0

    untilJust $ do
        modifyAll arr (fmap (+1))
        whileJust_ (listToMaybe . map fst . filter (\x -> case snd x of Just x -> x >= 10
                                                                        Nothing -> False)
                                                                        <$> getAssocs arr)
            (incrNeighbors arr height width)
        modifySTRef iteration (+1)
        i <- readSTRef iteration
        currentArray <- getElems arr
        let result = if all (== Nothing) currentArray then Just i else Nothing
        modifyAll arr (\case Just x -> Just x ; Nothing -> Just 0)
        pure result

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

parseDay15 :: String -> [[Int]]
parseDay15 = parseDay9

astar :: (Eq l, Ord l, Show l) => (l -> [(Int, l)]) -> (l -> Int) -> l -> Int
astar neighbors heuristic startingLoc = astar' neighbors heuristic (Map.insert startingLoc 0 Map.empty) (H.insert (heuristic startingLoc, startingLoc) H.empty)
    where astar' :: (Eq l, Ord l, Show l) => (l -> [(Int, l)]) -> (l -> Int) -> Map.Map l Int -> H.MinPrioHeap Int l -> Int
          astar' neighbors heuristic bestKnown paths = let
                  ((_, loc), otherPaths) = fromJust $ H.view paths
                  score = fromJust $ Map.lookup loc bestKnown
                  neighborhood = filter (\(c, l) -> maybe True (>(c + score)) (Map.lookup l bestKnown)) $ neighbors loc
                  newPaths = map (\(cost, dest) -> (cost + score, dest)) neighborhood
                  newPathsH = (H.fromList $ map (\(c, p) -> (c + heuristic p, p)) newPaths)
              in
                  if heuristic loc == 0 then score else
                  astar' neighbors heuristic (foldr (\(v, k) bn -> Map.insert k v bn) bestKnown newPaths) (H.union newPathsH otherPaths)

solveDay15Part1 :: [[Int]] -> Int
solveDay15Part1 board =
    let arrayBoard = arrayFrom2dList board
        ((_, _), (height, width)) = bounds arrayBoard
        lookup = (arrayBoard !)
    in astar (neighbors lookup width height) (heuristic width height) (0, 0)
    where neighbors :: ((Int, Int)->Int) -> Int -> Int -> (Int, Int) -> [(Int, (Int, Int))]
          neighbors lookup width height (x, y) = map (\l -> (lookup l, l)) $
                filter (validBoardLoc width height) [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]

          validBoardLoc :: Int -> Int -> (Int, Int) -> Bool
          validBoardLoc width height (x, y) = x >= 0 && x <= width && y >= 0 && y <= height

          heuristic :: Int -> Int -> (Int, Int) -> Int
          heuristic width height (x, y) = width - x + height - y

solveDay15Part2 :: [[Int]] -> Int
solveDay15Part2 = solveDay15Part1 . expand
    where expand :: [[Int]] -> [[Int]]
          expand grid = concat [map (\row -> concat [map (\c -> ((c + x + y - 1) `rem` 9) + 1) row | x <- [0..4]]) grid
                                | y <- [0..4]]

parseDay16 :: String -> [[Bool]]
parseDay16 = map parseLine . lines
    where parseLine :: String -> [Bool]
          parseLine = concatMap hexToBits

          hexToBits :: Char -> [Bool]
          hexToBits '0' = [False,False,False,False]
          hexToBits '1' = [False,False,False,True]
          hexToBits '2' = [False,False,True,False]
          hexToBits '3' = [False,False,True,True]
          hexToBits '4' = [False,True,False,False]
          hexToBits '5' = [False,True,False,True]
          hexToBits '6' = [False,True,True,False]
          hexToBits '7' = [False,True,True,True]
          hexToBits '8' = [True,False,False,False]
          hexToBits '9' = [True,False,False,True]
          hexToBits 'A' = [True,False,True,False]
          hexToBits 'B' = [True,False,True,True]
          hexToBits 'C' = [True,True,False,False]
          hexToBits 'D' = [True,True,False,True]
          hexToBits 'E' = [True,True,True,False]
          hexToBits 'F' = [True,True,True,True]
          hexToBits _ = error "invalid hex"

boolBitsToInt :: [Bool] -> Int
boolBitsToInt = bitsToInt . map boolToBit

boolToBit :: Bool -> Int
boolToBit True = 1
boolToBit False = 0

newtype Day16Packet = Day16Packet(Int, Day16PacketType) deriving Show
data Day16PacketType = Literal Integer | Operator(Int, [Day16Packet]) deriving Show

parseDay16Packet :: [Bool] -> (Day16Packet, [Bool])
parseDay16Packet (v1:v2:v3:True:False:False:rest) = let
        (lit, next) = inner 0 rest
        in (Day16Packet(boolBitsToInt [v1, v2, v3], Literal lit ), next)
    where inner :: Integer -> [Bool] -> (Integer, [Bool])
          inner prev (True:b1:b2:b3:b4:rest) = inner (prev * 16 + toInteger (boolBitsToInt [b1, b2, b3, b4])) rest
          inner prev (False:b1:b2:b3:b4:rest) = (prev * 16 + toInteger (boolBitsToInt [b1, b2, b3, b4]), rest)
          inner _ _ = error "Malformed packet"
parseDay16Packet (v1:v2:v3:t1:t2:t3:rest) = (Day16Packet(boolBitsToInt [v1, v2, v3], Operator(boolBitsToInt [t1, t2, t3], fst (inner rest))), snd (inner rest))
    where inner :: [Bool] -> ([Day16Packet], [Bool])
          inner (True:rest) = let totalNum = boolBitsToInt (take 11 rest)
                                  packetData = drop 11 rest
                              in parseNPackets totalNum [] packetData
          inner (False:rest) = let len = boolBitsToInt (take 15 rest)
                                   packetData = take len (drop 15 rest)
                                   remaining = drop len (drop 15 rest)
                               in (parsePackets [] packetData, remaining)
          inner [] = error "Empty payload"

          parsePackets :: [Day16Packet] -> [Bool] -> [Day16Packet]
          parsePackets prev [] = reverse prev
          parsePackets prev d = let (pack, next) = parseDay16Packet d
                                in parsePackets (pack:prev) next

          parseNPackets :: Int -> [Day16Packet] -> [Bool] -> ([Day16Packet], [Bool])
          parseNPackets 0 packets d = (reverse packets, d)
          parseNPackets n prev d = let (p, next) = parseDay16Packet d
                                   in parseNPackets (n-1) (p:prev) next

parseDay16Packet _ = error "Packet must start with at least 6 bits"

solveDay16Part1 :: [[Bool]] -> Int
solveDay16Part1 = sum . map solveLine
    where solveLine :: [Bool] -> Int
          solveLine = sumVersions . fst . parseDay16Packet

          sumVersions :: Day16Packet -> Int
          sumVersions (Day16Packet(version, Literal _)) = version
          sumVersions (Day16Packet(version, Operator(_, packets))) = version + sum (map sumVersions packets)

solveDay16Part2 :: [[Bool]] -> Integer
solveDay16Part2 [packet] = eval $ fst $ parseDay16Packet packet
    where eval :: Day16Packet -> Integer
          eval (Day16Packet(_, Literal l)) = l
          eval (Day16Packet(_, Operator(op, children))) = getOp op $ map eval children

          getOp :: Int -> [Integer] -> Integer
          getOp 0 = sum
          getOp 1 = product
          getOp 2 = minimum
          getOp 3 = maximum
          getOp 5 = \[a, b] -> if a > b then 1 else 0
          getOp 6 = \[a, b] -> if a < b then 1 else 0
          getOp 7 = \[a, b] -> if a == b then 1 else 0
          getOp _ = error "Unknown operator"

solveDay16Part2 _ = error "More than one packet"

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

data SnailfishNumber = Leaf Int | Pair SnailfishNumber SnailfishNumber deriving Show

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

solvePuzzle = parseDay18

main :: IO ()
main = do
        handle <- openFile "data/day18.txt" ReadMode
        contents <- hGetContents handle
        print $ solvePuzzle contents
        hClose handle