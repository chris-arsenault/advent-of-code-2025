{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Graph.Inductive.Graph (mkGraph, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (components)
import Data.List (sortBy)
import qualified Data.IntMap.Strict as IM
import System.CPUTime (getCPUTime)

type Point = (Int, Int, Int)

loadPoints :: [String] -> [Point]
loadPoints lns = map parse $ filter (not . null) lns
  where
    parse line = let [x,y,z] = map read (words [if c==',' then ' ' else c | c <- line])
                 in (x, y, z)

buildEdges :: [Point] -> [(Int, (Int, Int, Int))]
buildEdges pts = sortBy (\(d1,_) (d2,_) -> compare d1 d2) edges
  where
    n = length pts
    indexed = zip [0..] pts
    edges = [ let (xi,yi,zi) = pts !! i
                  (xj,yj,zj) = pts !! j
                  !dx = xi - xj; !dy = yi - yj; !dz = zi - zj
                  !d2 = dx*dx + dy*dy + dz*dz
              in (d2, (i, j, d2))
            | i <- [0..n-1], j <- [i+1..n-1] ]

-- Union-Find using Data.IntMap for parent tracking
data DSU = DSU { parent :: IM.IntMap Int, size :: IM.IntMap Int, comps :: Int }

newDSU :: Int -> DSU
newDSU n = DSU (IM.fromList [(i,i) | i <- [0..n-1]])
               (IM.fromList [(i,1) | i <- [0..n-1]])
               n

findDSU :: DSU -> Int -> (DSU, Int)
findDSU dsu x =
  let p = parent dsu IM.! x
  in if p == x
       then (dsu, x)
       else let (dsu', root) = findDSU dsu p
                dsu'' = dsu' { parent = IM.insert x root (parent dsu') }
            in (dsu'', root)

unionDSU :: DSU -> Int -> Int -> (DSU, Bool)
unionDSU dsu a b =
  let (dsu1, ra) = findDSU dsu a
      (dsu2, rb) = findDSU dsu1 b
  in if ra == rb
       then (dsu2, False)
       else let sa = size dsu2 IM.! ra
                sb = size dsu2 IM.! rb
                (ra', rb') = if sa < sb then (rb, ra) else (ra, rb)
                sa' = sa + sb
                newParent = IM.insert rb' ra' (parent dsu2)
                newSize = IM.insert ra' sa' (size dsu2)
            in (DSU newParent newSize (comps dsu2 - 1), True)

part1 :: Int -> [(Int, (Int, Int, Int))] -> Int -> Int
part1 n edges k =
  let limit = min k (length edges)
      dsu0 = newDSU n
      dsuFinal = foldl (\d (_, (a, b, _)) -> fst (unionDSU d a b)) dsu0 (take limit edges)
      sizes = [size dsuFinal IM.! i | i <- [0..n-1], let (d', r) = findDSU dsuFinal i in r == i]
      sorted = reverse $ sortBy compare sizes
      padded = sorted ++ repeat 1
  in (padded !! 0) * (padded !! 1) * (padded !! 2)

part2 :: [Point] -> [(Int, (Int, Int, Int))] -> Int
part2 pts edges =
  let n = length pts
      go dsu lastProd [] = lastProd
      go dsu lastProd ((_, (a, b, _)):rest)
        | comps dsu <= 1 = lastProd
        | otherwise =
            let (dsu', merged) = unionDSU dsu a b
            in if merged
                 then let (xa, _, _) = pts !! a
                          (xb, _, _) = pts !! b
                          !prod = xa * xb
                      in go dsu' prod rest
                 else go dsu' lastProd rest
  in go (newDSU n) 0 edges

main :: IO ()
main = do
    linesIn <- lines <$> readFile "input.txt"
    let pts = loadPoints linesIn
        edges = buildEdges pts
        n = length pts
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
