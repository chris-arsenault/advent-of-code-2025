{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (sortBy)
import System.CPUTime (getCPUTime)

type Point = (Int,Int,Int)

loadPoints :: [String] -> [Point]
loadPoints = map (\line -> let [x,y,z] = map read (splitComma line) in (x,y,z))
  where splitComma s = words [if c==',' then ' ' else c | c <- s]

data DSU = DSU { parent :: [Int], size :: [Int], components :: Int }

makeDSU :: Int -> DSU
makeDSU n = DSU [0..n-1] (replicate n 1) n

findRoot :: DSU -> Int -> (Int, DSU)
findRoot dsu x =
  let p = parent dsu !! x
  in if p == x then (x, dsu)
     else let (r, dsu') = findRoot dsu p
              parents' = replace (parent dsu') x r
          in (r, dsu'{parent = parents'})

unionDSU :: DSU -> Int -> Int -> (Bool, DSU)
unionDSU dsu a b =
  let (ra, dsu1) = findRoot dsu a
      (rb, dsu2) = findRoot dsu1 b
  in if ra == rb then (False, dsu2)
     else
       let sa = size dsu2 !! ra
           sb = size dsu2 !! rb
           (ra', rb') = if sa < sb then (rb, ra) else (ra, rb)
           parent' = replace (parent dsu2) rb' ra'
           size' = replace (size dsu2) ra' (sa + sb)
       in (True, dsu2{parent=parent', size=size', components=components dsu2 - 1})

replace :: [a] -> Int -> a -> [a]
replace xs idx val =
  let (a,_:b) = splitAt idx xs in a ++ val:b

buildEdges :: [Point] -> [(Int,Int,Int)]
buildEdges pts =
  let n = length pts
      edges = [ let (xi,yi,zi) = pts !! i
                    (xj,yj,zj) = pts !! j
                    dx = xi - xj; dy = yi - yj; dz = zi - zj
                    d2 = dx*dx + dy*dy + dz*dz
                in (d2, i, j)
              | i <- [0..n-1], j <- [i+1..n-1] ]
  in sortBy (\(a,_,_) (b,_,_) -> compare a b) edges

part1 :: Int -> [(Int,Int,Int)] -> Int -> Int
part1 n edges k =
  let dsu0 = makeDSU n
      dsuFinal = foldl (\dsu (_,a,b) -> snd (unionDSU dsu a b)) dsu0 (take k edges)
      roots = [ r | i <- [0..n-1], let (r, _) = findRoot dsuFinal i, r == i ]
      sizesList = [ size dsuFinal !! r | r <- roots ]
      top = take 3 (reverse (sortBy compare sizesList ++ repeat 1))
  in product top

part2 :: [Point] -> [(Int,Int,Int)] -> Int
part2 pts edges =
  let n = length pts
      dsu0 = makeDSU n
      go _ [] = 0
      go dsu ((_,a,b):es) =
        let (merged, dsu') = unionDSU dsu a b
            prodX = (fst3 (pts!!a)) * (fst3 (pts!!b))
        in if merged
             then if components dsu' == 1 then prodX else go dsu' es
             else go dsu' es
  in go dsu0 edges
  where fst3 (x,_,_) = x

main :: IO ()
main = do
  linesIn <- lines <$> readFile "input.txt"
  let pts = loadPoints linesIn
      edges = buildEdges pts
  t0 <- getCPUTime
  let p1 = part1 (length pts) edges 1000
      p2 = part2 pts edges
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "top3_product=" ++ show p1 ++ " final_join_x_product=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
