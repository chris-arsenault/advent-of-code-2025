{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isDigit)
import qualified Data.Vector as V
import System.CPUTime (getCPUTime)

loadGrid :: String -> V.Vector (V.Vector Char)
loadGrid text =
  let ls = filter (not . null) (lines text)
      w = maximum (map length ls)
      padded = map (\l -> l ++ replicate (w - length l) ' ') ls
  in V.fromList (map V.fromList padded)

splitBlocks :: V.Vector (V.Vector Char) -> [(Int,Int)]
splitBlocks grid =
  let h = V.length grid
      w = if h > 0 then V.length (grid V.! 0) else 0
      emptyCol c = all (\r -> (grid V.! r) V.! c == ' ') [0..h-1]
      cols = V.fromList $ map emptyCol [0..w-1]
  in collect cols 0 w
  where
    collect cols c w
      | c >= w = []
      | cols V.! c = collect cols (c+1) w
      | otherwise =
          let start = c
              end = findEnd cols c w
          in (start,end):collect cols end w
    findEnd cols c w
      | c >= w = w
      | cols V.! c = c
      | otherwise = findEnd cols (c+1) w

problemOp :: V.Vector Char -> Int -> Int -> Char
problemOp row s e =
  case filter (`elem` "+*") (V.toList (V.slice s (e-s) row)) of
    (x:_) -> x
    _ -> '+'

evalNums :: [Integer] -> Char -> Integer
evalNums nums op = case op of
  '+' -> sum nums
  _   -> product nums

part1 :: V.Vector (V.Vector Char) -> [(Int,Int)] -> Integer
part1 grid blocks =
  let opRow = grid V.! (V.length grid - 1)
      numRows = V.length grid - 1
  in sum [ let op = problemOp opRow s e
               nums = [ read (filter (/=' ') $ V.toList (V.slice s (e-s) (grid V.! r)))
                      | r <- [0..numRows-1]
                      , let slice = V.toList (V.slice s (e-s) (grid V.! r))
                      , any isDigit slice ]
           in evalNums nums op
         | (s,e) <- blocks ]

part2 :: V.Vector (V.Vector Char) -> [(Int,Int)] -> Integer
part2 grid blocks =
  let h = V.length grid - 1
      opRow = grid V.! (V.length grid - 1)
  in sum [ let op = problemOp opRow s e
               nums = collectNums h s e
           in evalNums nums op
         | (s,e) <- blocks ]
  where
    collectNums h s e =
      [ read digits  -- No reverse: top-to-bottom = most significant first
      | c <- reverse [s..e-1]
      , let digits = [ (grid V.! r) V.! c | r <- [0..h-1], isDigit ((grid V.! r) V.! c) ]
      , not (null digits)
      ]

main :: IO ()
main = do
  t0 <- getCPUTime
  text <- readFile "input.txt"
  let grid = loadGrid text
      blocks = splitBlocks grid
  let !p1 = part1 grid blocks
      !p2 = part2 grid blocks
  t1 <- getCPUTime
  let elapsedMs = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "grand_total=" ++ show p1 ++ " quantum_total=" ++ show p2
             ++ " elapsed_ms=" ++ showFF elapsedMs

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
