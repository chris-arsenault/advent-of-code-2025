{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isSpace)
import System.CPUTime (getCPUTime)
import Data.List (sortBy)
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.IntervalMap.Interval (Interval(..))

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOnBlank :: String -> ([String],[String])
splitOnBlank text =
  let ls = map trim (lines text)
      (top,bottom) = break null ls
  in (top, drop 1 bottom)

parseInput :: String -> ([(Integer,Integer)], [Integer])
parseInput text =
  let (top,bottom) = splitOnBlank text
      ranges = [ (read a, read b')
               | l <- top
               , not (null l)
               , let (a,rest) = break (=='-') l
               , not (null rest)
               , let b' = drop 1 rest
               , not (null b')
               ]
      ids = [ read l | l <- bottom, not (null l) ]
  in (ranges, ids)

-- Build an IntervalMap from ranges (for fast point queries)
buildIntervalMap :: [(Integer,Integer)] -> IM.IntervalMap (Interval Integer) ()
buildIntervalMap ranges =
  IM.fromList [(ClosedInterval lo hi, ()) | (lo, hi) <- ranges]

-- Check if a point is contained in any interval
containsPoint :: IM.IntervalMap (Interval Integer) () -> Integer -> Bool
containsPoint im x = not $ null $ IM.containing im x

-- Merge overlapping intervals for Part 2 (IntervalMap doesn't auto-merge)
mergeRanges :: [(Integer,Integer)] -> [(Integer,Integer)]
mergeRanges rs =
  let sorted = sortBy (\(a,_) (b,_) -> compare a b) rs
  in go sorted
  where
    go [] = []
    go [x] = [x]
    go ((a1,b1):(a2,b2):rest)
      | a2 <= b1 + 1 = go ((a1, max b1 b2):rest)
      | otherwise = (a1,b1) : go ((a2,b2):rest)

solve :: String -> (Integer, Integer)
solve text =
  let (ranges, ids) = parseInput text
      im = buildIntervalMap ranges  -- IntervalMap for O(log n) point queries
      merged = mergeRanges ranges   -- Merged intervals for Part 2
      -- Part 1: Count IDs in any interval (uses IntervalMap)
      fresh = fromIntegral $ length [() | i <- ids, containsPoint im i]
      -- Part 2: Total span of merged intervals
      total = sum [hi - lo + 1 | (lo, hi) <- merged]
  in (fresh, total)

main :: IO ()
main = do
  text <- readFile "input.txt"
  t0 <- getCPUTime
  let !(p1, p2) = solve text
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "available_fresh=" ++ show p1 ++ " total_fresh_ids=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
