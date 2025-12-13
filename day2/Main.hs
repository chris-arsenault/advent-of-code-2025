{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isDigit, isSpace)
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Check if a number has its first half equal to second half (even length only)
-- e.g., 1212 -> 12 == 12 -> True
isRepeatedHalf :: Integer -> Bool
isRepeatedHalf n
  | n <= 0 = False
  | otherwise =
      let s = show n
          len = length s
      in len >= 2 && even len &&
         let half = len `div` 2
             (first, second) = splitAt half s
         in first == second

-- Check if a number is composed of a repeated substring pattern
-- e.g., 123123 -> "123" repeated 2 times -> True
-- e.g., 1111 -> "1" repeated 4 times -> True
isRepeatedPattern :: Integer -> Bool
isRepeatedPattern n
  | n <= 0 = False
  | otherwise =
      let s = show n
          len = length s
      in any (checkPattern s len) [1 .. len - 1]
  where
    checkPattern s len subLen
      | len `mod` subLen /= 0 = False
      | otherwise =
          let pat = take subLen s
              reps = len `div` subLen
          in concat (replicate reps pat) == s

-- Parse ranges from input, handling both comma and newline separators
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

-- Split string on any of the given delimiter characters
splitOn :: String -> String -> [String]
splitOn _ [] = [""]
splitOn delims (c:cs)
  | c `elem` delims = "" : rest
  | otherwise = let (w:ws) = rest in (c:w) : ws
  where rest = splitOn delims cs

-- Sum numbers in range that satisfy a predicate
sumInRange :: (Integer -> Bool) -> Integer -> Integer -> Integer
sumInRange pred lo hi = go lo 0
  where
    go !i !acc
      | i > hi = acc
      | pred i = go (i + 1) (acc + i)
      | otherwise = go (i + 1) acc

solve :: String -> (Integer, Integer)
solve text =
  let ranges = parseRanges text
      !p1 = sum [sumInRange isRepeatedHalf lo hi | (lo, hi) <- ranges]
      !p2 = sum [sumInRange isRepeatedPattern lo hi | (lo, hi) <- ranges]
  in (p1, p2)

main :: IO ()
main = do
  text <- readFile "input.txt"
  t0 <- getCPUTime
  let !(p1, p2) = solve text
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "repeated-halves-sum=" ++ show p1 ++ " repeated-pattern-sum=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
