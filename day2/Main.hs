{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isDigit, isSpace)
import System.CPUTime (getCPUTime)
import Data.List (sort)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

digitsLen :: Int -> Int
digitsLen n | n == 0 = 1
            | otherwise = floor (logBase 10 (fromIntegral n :: Double)) + 1

pow10 :: Int -> Int
pow10 k = 10 ^ k

generateEvenHalf :: Int -> [Int]
generateEvenHalf maxn =
  let maxlen = digitsLen maxn
      vals = [ let n = num * mul + num
               in n
             | half <- [1 .. maxlen `div` 2]
             , let start = pow10 (half - 1)
             , let limit = pow10 half
             , let mul = pow10 half
             , num <- [start .. limit - 1]
             , let n = num * mul + num
             , n <= maxn
             ]
  in sort vals

generatePeriodic :: Int -> [Int]
generatePeriodic maxn =
  let maxlen = digitsLen maxn
      vals = [ n
             | baseLen <- [1 .. (maxlen + 1) `div` 2]
             , let start = pow10 (baseLen - 1)
             , let limit = pow10 baseLen
             , base <- [start .. limit - 1]
             , let baseS = show base
             , reps <- [2 .. maxlen `div` baseLen]
             , let s = concat (replicate reps baseS)
             , let n = read s
             , n <= maxn
             ]
  in sort vals

prefixSums :: [Int] -> [Int]
prefixSums = scanl (+) 0

rangeSum :: [Int] -> [Int] -> Int -> Int -> Int
rangeSum v ps lo hi =
  let i = lowerBound v lo
      j = upperBound v hi
  in if j < i then 0 else ps !! j - ps !! i

-- lowerBound: first index >= x
lowerBound :: Ord a => [a] -> a -> Int
lowerBound xs x = lb 0 (length xs)
  where
    lb l h | l >= h = l
           | otherwise =
               let m = (l + h) `div` 2
               in if xs !! m < x then lb (m+1) h else lb l m

upperBound :: Ord a => [a] -> a -> Int
upperBound xs x = ub 0 (length xs)
  where
    ub l h | l >= h = l
           | otherwise =
               let m = (l + h) `div` 2
               in if xs !! m <= x then ub (m+1) h else ub l m

parseRanges :: String -> [(Int, Int)]
parseRanges text =
  [ let (a,b) = break (== '-') (trim part)
    in (read a, read (tail b))
  | part <- splitDelims text ",\n"
  , not (null (trim part))
  ]

splitDelims :: String -> String -> [String]
splitDelims _ [] = [""]
splitDelims delims (c:cs)
  | c `elem` delims =
      "":rest
  | otherwise =
      let (w:ws) = splitDelims delims cs
      in (c:w):ws
  where rest = splitDelims delims cs

solve :: String -> (Int, Int)
solve text =
  let ranges = parseRanges text
      maxn = maximum (map snd ranges)
      evens = generateEvenHalf maxn
      evps = prefixSums evens
      per = generatePeriodic maxn
      perps = prefixSums per
      p1 = sum [rangeSum evens evps a b | (a,b) <- ranges]
      p2 = sum [rangeSum per perps a b | (a,b) <- ranges]
  in (p1,p2)

main :: IO ()
main = do
  text <- readFile "input.txt"
  t0 <- getCPUTime
  let (p1,p2) = solve text
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "repeated-halves-sum=" ++ show p1 ++ " repeated-pattern-sum=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
