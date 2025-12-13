{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Map.Strict as M
import Data.Char (isSpace)
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

loadGraph :: [String] -> M.Map String [String]
loadGraph = foldl addLine M.empty
  where
    addLine m line =
      let t = trim line
      in if null t then m
         else let (l,r) = break (==':') t
                  src = trim l
                  dests = words (trim (drop 1 r))
              in M.insert src dests m

countPaths :: M.Map String [String] -> String -> String -> Int
countPaths g start target = go M.empty start
  where
    go memo node
      | node == target = 1
      | otherwise =
          case M.lookup node memo of
            Just v -> v
            Nothing ->
              let total = sum [ go memo' nxt | nxt <- M.findWithDefault [] node g
                                             , let memo' = memo ]
                  memo'' = M.insert node total memo
              in total

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  t0 <- getCPUTime
  let g = loadGraph ls
      p1 = countPaths g "you" "out"
      a1 = countPaths g "svr" "dac"
      a2 = countPaths g "dac" "fft"
      a3 = countPaths g "fft" "out"
      b1 = countPaths g "svr" "fft"
      b2 = countPaths g "fft" "dac"
      b3 = countPaths g "dac" "out"
      p2 = a1*a2*a3 + b1*b2*b3
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "paths_you_to_out=" ++ show p1 ++ " paths_svr_via_dac_fft=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
