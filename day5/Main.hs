{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isSpace)
import System.CPUTime (getCPUTime)
import Data.List (sortBy)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Split lines, trimming each line first to handle CRLF
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

-- Binary search to check if value is in any merged interval
inAny :: [(Integer,Integer)] -> Integer -> Bool
inAny [] _ = False
inAny rs x = search 0 (length rs - 1)
  where
    search lo hi
      | lo > hi = False
      | otherwise =
          let mid = (lo + hi) `div` 2
              (a, b) = rs !! mid
          in if x < a then search lo (mid - 1)
             else if x > b then search (mid + 1) hi
             else True

solve :: String -> (Integer, Integer)
solve text =
  let (ranges, ids) = parseInput text
      merged = mergeRanges ranges
      fresh = fromIntegral $ length [() | i <- ids, inAny merged i]
      total = sum [b - a + 1 | (a,b) <- merged]
  in (fresh, total)

main :: IO ()
main = do
  text <- readFile "input.txt"
  t0 <- getCPUTime
  let (p1, p2) = solve text
  p1 `seq` p2 `seq` return ()
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "available_fresh=" ++ show p1 ++ " total_fresh_ids=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
