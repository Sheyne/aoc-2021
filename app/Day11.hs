{-# LANGUAGE LambdaCase #-}

module Day11 where

import Helpers
import Control.Monad.ST
import Control.Monad.Loops
import Data.Array.ST
import Data.Maybe
import Data.Char
import Data.Array.IArray
import Data.STRef
import Data.IORef (modifyIORef)


parseDay11 :: String -> [[Int]]
parseDay11 = map (map digitToInt) . lines

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
