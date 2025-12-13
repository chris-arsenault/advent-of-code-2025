{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isDigit)
import Data.List (transpose)
import System.CPUTime (getCPUTime)

loadGrid :: String -> [String]
loadGrid text =
  let ls = filter (not . null) (lines text)
      w = maximum (map length ls)
  in map (\l -> l ++ replicate (w - length l) ' ') ls

splitBlocks :: [String] -> [(Int,Int)]
splitBlocks grid =
  let h = length grid
      w = length (head grid)
      emptyCol c = all (\r -> (grid !! r) !! c == ' ') [0..h-1]
      cols = map emptyCol [0..w-1]
  in collect cols 0 w
  where
    collect cols c w
      | c >= w = []
      | cols !! c = collect cols (c+1) w
      | otherwise =
          let start = c
              end = findEnd cols c w
          in (start,end):collect cols end w
    findEnd cols c w
      | c >= w = w
      | cols !! c = c
      | otherwise = findEnd cols (c+1) w

problemOp :: String -> Int -> Int -> Char
problemOp row s e =
  case filter (`elem` "+*") (take (e-s) (drop s row)) of
    (x:_) -> x
    _ -> '+'

evalNums :: [Int] -> Char -> Int
evalNums nums op = case op of
  '+' -> sum nums
  _   -> product nums

part1 :: [String] -> [(Int,Int)] -> Int
part1 grid blocks =
  let opRow = last grid
      rows = init grid
  in sum [ let op = problemOp opRow s e
               nums = [ read t | row <- rows
                               , let t = dropWhile (==' ') (take (e-s) (drop s row))
                               , not (null t) ]
           in evalNums nums op
         | (s,e) <- blocks ]

part2 :: [String] -> [(Int,Int)] -> Int
part2 grid blocks =
  let h = length grid - 1
      opRow = last grid
  in sum [ let op = problemOp opRow s e
               nums = collectNums h s e
           in evalNums nums op
         | (s,e) <- blocks ]
  where
    collectNums h s e =
      [ read (reverse digits)
      | c <- reverse [s..e-1]
      , let digits = [ grid !! r !! c | r <- [0..h-1], isDigit (grid !! r !! c) ]
      , not (null digits)
      ]

main :: IO ()
main = do
  text <- readFile "input.txt"
  let grid = loadGrid text
      blocks = splitBlocks grid
  t0 <- getCPUTime
  let p1 = part1 grid blocks
      p2 = part2 grid blocks
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "grand_total=" ++ show p1 ++ " quantum_total=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
