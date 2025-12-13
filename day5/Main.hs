{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isSpace)
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOnBlank :: String -> ([String],[String])
splitOnBlank text =
  let ls = lines text
      (top,bottom) = break null ls
  in (top, drop 1 bottom)

parseInput :: String -> ([(Int,Int)], [Int])
parseInput text =
  let (top,bottom) = splitOnBlank text
      ranges = [ let (a,b) = break (=='-') (trim l)
                 in (read a, read (tail b))
               | l <- top, not (null (trim l)) ]
      ids    = [ read (trim l) | l <- bottom, not (null (trim l)) ]
  in (ranges, ids)

mergeRanges :: [(Int,Int)] -> [(Int,Int)]
mergeRanges rs =
  let sorted = sortByFst rs
  in foldl step [] sorted
  where
    sortByFst = sortOn fst
    step [] x = [x]
    step acc@(y:ys) (a,b)
      | a > snd y + 1 = (a,b):acc
      | otherwise     = (fst y, max (snd y) b):ys

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortPairs . map (\x -> (f x, x))
  where sortPairs = sortBy (\(a,_) (b,_) -> compare a b)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr insert []
  where insert x [] = [x]
        insert x ys@(y:ys')
          | cmp x y == GT = y : insert x ys'
          | otherwise     = x : ys

inAny :: [(Int,Int)] -> Int -> Bool
inAny rs x = go rs
  where
    go [] = False
    go ((a,b):rest)
      | x < a = False
      | x > b = go rest
      | otherwise = True

solve :: String -> (Int, Int)
solve text =
  let (ranges, ids) = parseInput text
      merged = reverse (mergeRanges ranges) -- mergeRanges builds reversed
      fresh = length [() | i <- ids, inAny merged i]
      total = sum [b - a + 1 | (a,b) <- merged]
  in (fresh, total)

main :: IO ()
main = do
  text <- readFile "input.txt"
  t0 <- getCPUTime
  let (p1,p2) = solve text
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "available_fresh=" ++ show p1 ++ " total_fresh_ids=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
