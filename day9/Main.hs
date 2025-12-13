{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (tails)
import System.CPUTime (getCPUTime)

type P = (Int,Int)

loadPoints :: [String] -> [P]
loadPoints = map (\line -> let [x,y] = map read (splitComma line) in (x,y))
  where splitComma s = words [if c==',' then ' ' else c | c <- s]

maxRectAny :: [P] -> Int
maxRectAny pts =
  maximum (0:[ area p q | (p:rest) <- tails pts, q <- rest, area p q > 0 ])
  where area (x1,y1) (x2,y2) =
          if x1 == x2 || y1 == y2 then 0 else abs (x1 - x2) * abs (y1 - y2)

orientation :: P -> P -> P -> Int
orientation (x1,y1) (x2,y2) (x3,y3) =
  let val = (y2-y1)*(x3-x2) - (x2-x1)*(y3-y2)
  in if val > 0 then 1 else if val < 0 then -1 else 0

onSegment :: P -> P -> P -> Bool
onSegment (x1,y1) (x2,y2) (x3,y3) =
  x2 >= min x1 x3 && x2 <= max x1 x3 && y2 >= min y1 y3 && y2 <= max y1 y3

intersects :: P -> P -> P -> P -> Bool
intersects p1 q1 p2 q2 =
  let o1 = orientation p1 q1 p2
      o2 = orientation p1 q1 q2
      o3 = orientation p2 q2 p1
      o4 = orientation p2 q2 q1
      cross = o1 /= o2 && o3 /= o4
      col = (o1 == 0 && onSegment p1 p2 q1) ||
            (o2 == 0 && onSegment p1 q2 q1) ||
            (o3 == 0 && onSegment p2 p1 q2) ||
            (o4 == 0 && onSegment p2 q1 q2)
  in cross || col

pointInPoly :: [P] -> P -> Bool
pointInPoly poly (x,y) =
  let n = length poly
      go i j inside
        | i >= n = inside
        | otherwise =
            let (xi,yi) = poly !! i
                (xj,yj) = poly !! j
                inside' = if yi /= yj && y >= min yi yj && y < max yi yj
                            then let xint = xi + (y - yi) * (xj - xi) `div` (yj - yi)
                                 in if xint < x then not inside else inside
                            else inside
            in go (i+1) i inside'
  in go 0 (n-1) False

rectangleInside :: [P] -> [P] -> Bool
rectangleInside poly corners =
  all (pointInPoly poly) corners &&
  let edges = zip poly (tail (cycle poly))
      rectEdges = zip corners (tail (cycle corners))
  in not (any (\(a,b) -> any (\(p,q) -> intersects a b p q) edges) rectEdges)

maxRectInside :: [P] -> [P] -> Int
maxRectInside pts poly =
  maximum (0:
    [ let xmin = min x1 x2; xmax = max x1 x2
          ymin = min y1 y2; ymax = max y1 y2
          corners = [(xmin,ymin),(xmin,ymax),(xmax,ymax),(xmax,ymin)]
      in if rectangleInside poly corners then (xmax-xmin)*(ymax-ymin) else 0
    | (p:rest) <- tails pts
    , q <- rest
    , let (x1,y1)=p; let (x2,y2)=q
    , x1 /= x2, y1 /= y2 ])

main :: IO ()
main = do
  pts <- loadPoints <$> lines <$> readFile "input.txt"
  t0 <- getCPUTime
  let p1 = maxRectAny pts
      p2 = maxRectInside pts pts
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "max_rect_area=" ++ show p1 ++ " max_green_rect_area=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
