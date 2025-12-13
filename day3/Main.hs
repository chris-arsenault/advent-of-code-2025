{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (digitToInt, isSpace)
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

bestTwo :: String -> Int
bestTwo s =
  let digs = map digitToInt s
      n = length digs
  in if n < 2 then 0
     else let suffix = buildSuffix digs n
              go best i
                | i >= n-1 = best
                | otherwise =
                    let cand = 10 * digs !! i + suffix !! (i+1)
                    in go (max best cand) (i+1)
          in go (-1) 0

buildSuffix :: [Int] -> Int -> [Int]
buildSuffix digs n =
  let arr = replicate (n+1) 0
  in suffixFill arr digs (n-1)
  where
    suffixFill acc _ (-1) = acc
    suffixFill acc ds i =
      let v = max (acc !! (i+1)) (ds !! i)
          acc' = take i acc ++ [v] ++ drop (i+1) acc
      in suffixFill acc' ds (i-1)

bestK :: String -> Int -> Int
bestK s k =
  let digs = map digitToInt s
      dropN = length digs - k
      stack = buildStack [] dropN digs
      trimmed = take k stack
  in read (concatMap show trimmed)

buildStack :: [Int] -> Int -> [Int] -> [Int]
buildStack st dropN [] = st
buildStack st dropN (d:ds)
  | dropN > 0 && not (null st) && last st < d = buildStack (init st) (dropN-1) (d:ds)
  | otherwise = buildStack (st ++ [d]) dropN ds

solve :: [String] -> Int -> (Int, Int)
solve ls k =
  let go (!p1,!p2) [] = (p1,p2)
      go (!p1,!p2) (l:xs) =
        let t = trim l
        in if null t then go (p1,p2) xs
           else go (p1 + bestTwo t, p2 + bestK t k) xs
  in go (0,0) ls

main :: IO ()
main = do
  linesIn <- lines <$> readFile "input.txt"
  t0 <- getCPUTime
  let !(p1,p2) = solve linesIn 12
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "max-2-digit-sum=" ++ show p1 ++ " max-12-digit-sum=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
