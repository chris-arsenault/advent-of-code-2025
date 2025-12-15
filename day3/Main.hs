{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (digitToInt, isSpace)
import GHC.Clock (getMonotonicTimeNSec)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Part 1: Find best 2-digit number using suffix max array
bestTwo :: String -> Int
bestTwo s
  | length s < 2 = 0
  | otherwise =
      let digs = map digitToInt s
          n = length digs
          -- Build suffix max: suffixMax[i] = max of digs[i..n-1]
          suffixMax = scanr max 0 digs
          -- Find best: for each position i, candidate = digs[i]*10 + suffixMax[i+1]
          candidates = zipWith (\d sm -> d * 10 + sm) digs (tail suffixMax)
      in maximum ((-1) : init candidates)  -- init to exclude last (no second digit after it)

-- Part 2: Greedy monotonic stack algorithm
-- Use reversed stack: top of stack is head (O(1) access)
bestK :: String -> Int -> Int
bestK s k =
  let digs = map digitToInt s
      dropN = length digs - k
      -- Build stack with reversed representation (top = head)
      finalStack = buildStack [] dropN digs
      -- Take first k, reverse to get correct order, convert to number
      kept = take k (reverse finalStack)
  in digitsToInt kept

-- Stack is reversed: head is top. Push = prepend, pop = tail, peek = head
buildStack :: [Int] -> Int -> [Int] -> [Int]
buildStack !st !dropN [] = st
buildStack !st !dropN (d:ds)
  | dropN > 0, not (null st), head st < d = buildStack (tail st) (dropN - 1) (d:ds)
  | otherwise = buildStack (d : st) dropN ds

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc d -> acc * 10 + d) 0

solve :: [String] -> Int -> (Int, Int)
solve ls k = go 0 0 ls
  where
    go !p1 !p2 [] = (p1, p2)
    go !p1 !p2 (l:rest) =
      let t = trim l
      in if null t
         then go p1 p2 rest
         else go (p1 + bestTwo t) (p2 + bestK t k) rest

main :: IO ()
main = do
  t0 <- getMonotonicTimeNSec
  linesIn <- lines <$> readFile "input.txt"
  let !(p1, p2) = solve linesIn 12
  p1 `seq` p2 `seq` return ()
  t1 <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  putStrLn $ "max-2-digit-sum=" ++ show p1 ++ " max-12-digit-sum=" ++ show p2
             ++ " elapsed_ms=" ++ showFF elapsedMs
  where
    showFF x = let s = show (fromIntegral (round (x * 1000 :: Double)) / 1000 :: Double)
               in if '.' `elem` s then s else s ++ ".0"
