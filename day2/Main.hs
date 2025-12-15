{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isDigit, isSpace)
import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Generate all even-half numbers up to maxN
-- These are numbers like 1212, 123123 where first half equals second half
generateEvenHalf :: Integer -> [Integer]
generateEvenHalf maxN =
    [ n
    | halfLen <- [1 .. maxLen `div` 2]
    , let start = 10 ^ (halfLen - 1)
    , let end = 10 ^ halfLen
    , t <- [start .. end - 1]
    , let n = t * (10 ^ halfLen) + t
    , n <= maxN
    ]
  where
    maxLen = length (show maxN)

-- Generate all periodic numbers up to maxN
-- These are numbers where a base pattern repeats k >= 2 times
generatePeriodic :: Integer -> [Integer]
generatePeriodic maxN =
    Set.toList $ Set.fromList
        [ n
        | baseLen <- [1 .. (maxLen + 1) `div` 2]
        , let start = 10 ^ (baseLen - 1)
        , let end = 10 ^ baseLen
        , base <- [start .. end - 1]
        , reps <- [2 .. maxLen `div` baseLen]
        , let n = buildRepeated base baseLen reps
        , n <= maxN
        ]
  where
    maxLen = length (show maxN)

-- Build a number by repeating base `reps` times
buildRepeated :: Integer -> Int -> Int -> Integer
buildRepeated base baseLen reps = go reps 0
  where
    multiplier = 10 ^ baseLen
    go 0 !acc = acc
    go !r !acc = go (r - 1) (acc * multiplier + base)

-- Parse ranges from input
parseRanges :: String -> [(Integer, Integer)]
parseRanges text =
    [ (read a, read (tail b))
    | part <- splitOn ",\n" text
    , let trimmed = trim part
    , not (null trimmed)
    , let (a, b) = break (== '-') trimmed
    , not (null a)
    , not (null b)
    ]

splitOn :: String -> String -> [String]
splitOn _ [] = [""]
splitOn delims (c:cs)
    | c `elem` delims = "" : rest
    | otherwise = let (w:ws) = rest in (c:w) : ws
  where rest = splitOn delims cs

-- Binary search helpers on sorted Vector
lowerBound :: V.Vector Integer -> Integer -> Int
lowerBound vec val = go 0 (V.length vec)
  where
    go !lo !hi
        | lo >= hi = lo
        | V.unsafeIndex vec mid < val = go (mid + 1) hi
        | otherwise = go lo mid
      where mid = (lo + hi) `div` 2

upperBound :: V.Vector Integer -> Integer -> Int
upperBound vec val = go 0 (V.length vec)
  where
    go !lo !hi
        | lo >= hi = lo
        | V.unsafeIndex vec mid <= val = go (mid + 1) hi
        | otherwise = go lo mid
      where mid = (lo + hi) `div` 2

-- Compute prefix sums
prefixSums :: V.Vector Integer -> V.Vector Integer
prefixSums vec = V.scanl' (+) 0 vec

-- Range sum using binary search and prefix sums
rangeSum :: V.Vector Integer -> V.Vector Integer -> Integer -> Integer -> Integer
rangeSum arr prefix lo hi =
    let i = lowerBound arr lo
        j = upperBound arr hi
    in V.unsafeIndex prefix j - V.unsafeIndex prefix i

solve :: String -> (Integer, Integer)
solve text =
    let ranges = parseRanges text
        maxN = maximum (map snd ranges)

        -- Generate and sort even-half numbers
        evenList = sort $ generateEvenHalf maxN
        evenArr = V.fromList evenList
        evenPrefix = prefixSums evenArr

        -- Generate, deduplicate, and sort periodic numbers
        periodicList = sort $ generatePeriodic maxN
        periodicArr = V.fromList periodicList
        periodicPrefix = prefixSums periodicArr

        -- Compute sums for each range
        !p1 = sum [rangeSum evenArr evenPrefix lo hi | (lo, hi) <- ranges]
        !p2 = sum [rangeSum periodicArr periodicPrefix lo hi | (lo, hi) <- ranges]
    in (p1, p2)

main :: IO ()
main = do
    t0 <- getCPUTime
    text <- readFile "input.txt"
    let !(p1, p2) = solve text
    t1 <- getCPUTime
    let elapsedMs = fromIntegral (t1 - t0) / 1e9 :: Double
    putStrLn $ "repeated-halves-sum=" ++ show p1 ++ " repeated-pattern-sum=" ++ show p2
               ++ " elapsed_ms=" ++ showFF elapsedMs

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
