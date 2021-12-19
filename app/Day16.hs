module Day16 where

import Helpers


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
