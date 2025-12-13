{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.CPUTime (getCPUTime)

type Point = (Int, Int, Int)

loadPoints :: [String] -> V.Vector Point
loadPoints lns = V.fromList $ map parse $ filter (not . null) lns
  where
    parse line = let [x,y,z] = map read (words [if c==',' then ' ' else c | c <- line])
                 in (x, y, z)

-- Build edges as unboxed vector of (dist², i, j) sorted by distance
buildEdges :: V.Vector Point -> U.Vector (Int, Int, Int)
buildEdges pts = U.fromList $ sortBy (\(a,_,_) (b,_,_) -> compare a b) edges
  where
    n = V.length pts
    edges = [ let (xi,yi,zi) = pts V.! i
                  (xj,yj,zj) = pts V.! j
                  !dx = xi - xj; !dy = yi - yj; !dz = zi - zj
                  !d2 = dx*dx + dy*dy + dz*dz
              in (d2, i, j)
            | i <- [0..n-1], j <- [i+1..n-1] ]

-- DSU with path compression in ST monad
findRoot :: MU.MVector s Int -> Int -> ST s Int
findRoot parent x = do
    p <- MU.unsafeRead parent x
    if p == x
        then return x
        else do
            r <- findRoot parent p
            MU.unsafeWrite parent x r  -- path compression
            return r

unionST :: MU.MVector s Int -> MU.MVector s Int -> Int -> Int -> ST s Bool
unionST parent size a b = do
    ra <- findRoot parent a
    rb <- findRoot parent b
    if ra == rb
        then return False
        else do
            sa <- MU.unsafeRead size ra
            sb <- MU.unsafeRead size rb
            let (ra', rb') = if sa < sb then (rb, ra) else (ra, rb)
            MU.unsafeWrite parent rb' ra'
            MU.unsafeWrite size ra' (sa + sb)
            return True

part1 :: Int -> U.Vector (Int, Int, Int) -> Int -> Int
part1 n edges k = runST $ do
    parent <- MU.generate n id
    size   <- MU.replicate n 1
    let limit = min k (U.length edges)
    forM_ [0..limit-1] $ \i -> do
        let (_, a, b) = edges U.! i
        _ <- unionST parent size a b
        return ()
    -- Collect component sizes
    sizes <- U.generateM n $ \i -> do
        p <- MU.unsafeRead parent i
        if p == i
            then MU.unsafeRead size i
            else return 0
    let top3 = take 3 $ reverse $ sortBy compare $ filter (>0) $ U.toList sizes
        padded = top3 ++ repeat 1
    return $ (padded !! 0) * (padded !! 1) * (padded !! 2)

part2 :: V.Vector Point -> U.Vector (Int, Int, Int) -> Int
part2 pts edges = runST $ do
    let n = V.length pts
        edgeCount = U.length edges
    parent <- MU.generate n id
    size   <- MU.replicate n 1
    let go !comps !lastProd !i
          | i >= edgeCount = return lastProd
          | comps <= 1 = return lastProd
          | otherwise = do
              let (_, a, b) = edges U.! i
              merged <- unionST parent size a b
              if merged
                  then do
                      let (xa, _, _) = pts V.! a
                          (xb, _, _) = pts V.! b
                          !prod = xa * xb
                      go (comps - 1) prod (i + 1)
                  else go comps lastProd (i + 1)
    go n 0 0

main :: IO ()
main = do
    linesIn <- lines <$> readFile "input.txt"
    let pts = loadPoints linesIn
        edges = buildEdges pts
        n = V.length pts
    t0 <- getCPUTime
    let !p1 = part1 n edges 1000
        !p2 = part2 pts edges
    t1 <- getCPUTime
    let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
    putStrLn $ "top3_product=" ++ show p1 ++ " final_join_x_product=" ++ show p2 ++
               " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
