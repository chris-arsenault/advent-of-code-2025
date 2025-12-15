{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isSpace)
import GHC.Clock (getMonotonicTimeNSec)

simulate :: [String] -> (Int, Int, Int)
simulate = go 50 0 0
  where
    go !pos !zero !cross [] = (zero, cross, pos)
    go pos zero cross (raw:xs) =
      let line = trim raw
      in if null line
           then go pos zero cross xs
           else
             let sign = if head line == 'R' then 1 else -1
                 mag  = read (tail line) :: Int
                 first = let f = if sign == 1 then 100 - pos else pos
                         in if f == 0 then 100 else f
                 cross' = if mag >= first then cross + 1 + (mag - first) `div` 100 else cross
                 pos' = (pos + sign * mag) `mod` 100
                 zero' = if pos' == 0 then zero + 1 else zero
             in go pos' zero' cross' xs

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

main :: IO ()
main = do
  t0 <- getMonotonicTimeNSec
  input <- lines <$> readFile "input.txt"
  let !(z,c,p) = simulate input
  z `seq` c `seq` p `seq` return ()
  t1 <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  putStrLn $ "zero_landings=" ++ show z ++ " crossings=" ++ show c ++ " final_pos=" ++ show p
             ++ " elapsed_ms=" ++ showFF elapsedMs
  where
    showFF x = let s = show (fromIntegral (round (x * 1000 :: Double)) / 1000 :: Double)
               in if '.' `elem` s then s else s ++ ".0"
