{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Vector as V
import System.CPUTime (getCPUTime)

type Point = (Int, Int)

loadPoints :: [String] -> V.Vector Point
loadPoints lns = V.fromList $ map parse $ filter (not . null) lns
  where
    parse line = let [x,y] = map read (words [if c==',' then ' ' else c | c <- line])
                 in (x, y)

maxRectAny :: V.Vector Point -> Int
maxRectAny pts =
  let n = V.length pts
      go !best !i
        | i >= n = best
        | otherwise = go (goJ best i (i+1)) (i+1)
      goJ !best !i !j
        | j >= n = best
        | otherwise =
            let (x1,y1) = pts V.! i
                (x2,y2) = pts V.! j
                !dx = abs (x1 - x2)
                !dy = abs (y1 - y2)
                !area = (dx + 1) * (dy + 1)
                !best' = if area > best then area else best
            in goJ best' i (j+1)
  in go 0 0

-- Check if point (px,py) is on a horizontal or vertical edge
pointOnEdge :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
pointOnEdge px py x1 y1 x2 y2
  | x1 == x2 = px == x1 && py >= min y1 y2 && py <= max y1 y2
  | y1 == y2 = py == y1 && px >= min x1 x2 && px <= max x1 x2
  | otherwise = False

-- Ray casting with edge detection
pointInside :: Int -> Int -> V.Vector Point -> Bool
pointInside px py poly = go 0 (n-1) False
  where
    n = V.length poly
    go !i !j !inside
      | i >= n = inside
      | otherwise =
          let (x1,y1) = poly V.! j
              (x2,y2) = poly V.! i
          in if pointOnEdge px py x1 y1 x2 y2
             then True
             else let !cond = (y1 > py) /= (y2 > py)
                      !inside' = if cond && y1 /= y2
                                 then let !xInt = (x2 - x1) * (py - y1) `div` (y2 - y1) + x1
                                      in if px < xInt then not inside else inside
                                 else inside
                  in go (i+1) i inside'

-- Check if polygon edge crosses strictly inside the rectangle interior
edgeCrossesInterior :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
edgeCrossesInterior xlo xhi ylo yhi x1 y1 x2 y2
  | x1 == x2 = -- vertical edge
      x1 > xlo && x1 < xhi &&  -- strictly between left and right
      let ya = min y1 y2
          yb = max y1 y2
      in yb > ylo && ya < yhi  -- overlaps y range
  | y1 == y2 = -- horizontal edge
      y1 > ylo && y1 < yhi &&  -- strictly between bottom and top
      let xa = min x1 x2
          xb = max x1 x2
      in xb > xlo && xa < xhi  -- overlaps x range
  | otherwise = False

rectInsidePolygon :: Int -> Int -> Int -> Int -> V.Vector Point -> Bool
rectInsidePolygon xlo xhi ylo yhi poly =
  pointInside xlo ylo poly &&
  pointInside xlo yhi poly &&
  pointInside xhi ylo poly &&
  pointInside xhi yhi poly &&
  not (anyEdgeCrosses 0 (n-1))
  where
    n = V.length poly
    anyEdgeCrosses !i !j
      | i >= n = False
      | otherwise =
          let (x1,y1) = poly V.! j
              (x2,y2) = poly V.! i
          in edgeCrossesInterior xlo xhi ylo yhi x1 y1 x2 y2 ||
             anyEdgeCrosses (i+1) i

maxRectInside :: V.Vector Point -> V.Vector Point -> Int
maxRectInside pts poly =
  let n = V.length pts
      go !best !i
        | i >= n = best
        | otherwise = go (goJ best i (i+1)) (i+1)
      goJ !best !i !j
        | j >= n = best
        | otherwise =
            let (x1,y1) = pts V.! i
                (x2,y2) = pts V.! j
            in if x1 == x2 || y1 == y2
               then goJ best i (j+1)
               else let !xlo = min x1 x2
                        !xhi = max x1 x2
                        !ylo = min y1 y2
                        !yhi = max y1 y2
                        !best' = if rectInsidePolygon xlo xhi ylo yhi poly
                                 then let !area = (xhi - xlo + 1) * (yhi - ylo + 1)
                                      in if area > best then area else best
                                 else best
                    in goJ best' i (j+1)
  in go 0 0

main :: IO ()
main = do
  linesIn <- lines <$> readFile "input.txt"
  let pts = loadPoints linesIn
  t0 <- getCPUTime
  let !p1 = maxRectAny pts
      !p2 = maxRectInside pts pts
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "max_rect_area=" ++ show p1 ++ " max_green_rect_area=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
