{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Map.Strict as M
import Data.Char (isSpace)
import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)

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

-- Use IORef for mutable memoization
countPaths :: M.Map String [String] -> String -> String -> IO Integer
countPaths g start target = do
    memoRef <- newIORef M.empty
    doneRef <- newIORef M.empty
    go memoRef doneRef start
  where
    go memoRef doneRef node = do
        done <- readIORef doneRef
        case M.lookup node done of
            Just _ -> do
                memo <- readIORef memoRef
                return $ M.findWithDefault 0 node memo
            Nothing -> do
                modifyIORef' doneRef (M.insert node True)
                if node == target
                    then do
                        modifyIORef' memoRef (M.insert node 1)
                        return 1
                    else do
                        let neighbors = M.findWithDefault [] node g
                        counts <- mapM (go memoRef doneRef) neighbors
                        let !total = sum counts
                        modifyIORef' memoRef (M.insert node total)
                        return total

main :: IO ()
main = do
    startTime <- getMonotonicTimeNSec
    ls <- lines <$> readFile "input.txt"
    let !g = loadGraph ls
    !p1 <- countPaths g "you" "out"
    !a1 <- countPaths g "svr" "dac"
    !a2 <- countPaths g "dac" "fft"
    !a3 <- countPaths g "fft" "out"
    !b1 <- countPaths g "svr" "fft"
    !b2 <- countPaths g "fft" "dac"
    !b3 <- countPaths g "dac" "out"
    let !p2 = a1*a2*a3 + b1*b2*b3
    endTime <- getMonotonicTimeNSec
    let elapsedMs = fromIntegral (endTime - startTime) / 1e6 :: Double
    printf "paths_you_to_out=%d paths_svr_via_dac_fft=%d elapsed_ms=%.3f\n" p1 p2 elapsedMs
