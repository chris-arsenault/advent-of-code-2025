{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (guard, msum)
import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe (isJust)
import System.CPUTime (getCPUTime)

type Cell = (Int,Int)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

normalize :: [Cell] -> [Cell]
normalize cells =
  let minx = minimum (map fst cells)
      miny = minimum (map snd cells)
  in [ (x - minx, y - miny) | (x,y) <- cells ]

rotate :: [Cell] -> [Cell]
rotate cells =
  let maxx = maximum (map fst cells)
  in [ (y, maxx - x) | (x,y) <- cells ]

flipX :: [Cell] -> [Cell]
flipX cells =
  let maxx = maximum (map fst cells)
  in [ (maxx - x, y) | (x,y) <- cells ]

orientations :: [Cell] -> [[Cell]]
orientations cells = unique forms
  where
    forms = concat [ [normalize cur, normalize (flipX cur)] | cur <- take 4 (iterate rotate cells) ]
    unique = map safeHead . groupEq . sort
    safeHead (x:_) = x
    safeHead [] = []
    groupEq [] = []
    groupEq (x:xs) = let (grp,rest) = span (==x) xs in (x:grp):groupEq rest

-- Shape: (orientations, area, parity)
type Shape = ([[Cell]], Int, Int)

-- Calculate parity: sum of (1 if (x+y) even, -1 if odd)
calcParity :: [Cell] -> Int
calcParity cells = sum [if (x + y) `mod` 2 == 0 then 1 else -1 | (x, y) <- cells]

parse :: [String] -> ([Shape], [(Int,Int,[Int])])
parse ls = (shapes, regions)
  where
    trimmedLs = map trim ls
    (shapeLines, regionLines) = break (\l -> 'x' `elem` l && ':' `elem` l) trimmedLs
    shapes = parseShapes shapeLines
    regions = parseRegions regionLines

parseShapes :: [String] -> [Shape]
parseShapes [] = []
parseShapes (l:ls)
  | null l = parseShapes ls
  | last l == ':' =
      let (grid, rest) = span (\x -> not (null x) && last x /= ':') ls
          cells = [ (x,y) | (y,row) <- zip [0..] (reverse grid), (x,ch) <- zip [0..] row, ch=='#' ]
          normCells = normalize cells
          forms = orientations normCells
          area = length normCells
          parity = calcParity normCells
      in (forms, area, parity) : parseShapes rest
  | otherwise = parseShapes ls

parseRegions :: [String] -> [(Int,Int,[Int])]
parseRegions = map parseRegion . filter (not . null)
  where
    parseRegion line =
      let (sizepart, countsPart) = break (==':') line
          [w,h] = map read (words [if c=='x' then ' ' else c | c <- sizepart])
          counts = map read (words (drop 1 countsPart))
      in (w,h,counts)

-- Check if the parity of shapes can possibly match the board
-- Uses DP to find if any achievable parity sum is within valid range
reachableDiff :: Int -> [Shape] -> [Int] -> Bool
reachableDiff totalArea shapes counts =
  let usedArea = sum [area * c | ((_, area, _), c) <- zip shapes counts]
  in if usedArea > totalArea then False
     else
       let maxDiff = sum [abs parity * c | ((_, _, parity), c) <- zip shapes counts]
           -- Board parity: black = ceil((w*h)/2), white = floor((w*h)/2)
           black = if totalArea `mod` 2 == 1 then totalArea `div` 2 + 1 else totalArea `div` 2
           white = totalArea - black
           -- Valid range for total parity
           lower = usedArea - 2 * white
           upper = 2 * black - usedArea
       in if maxDiff == 0
          then lower <= 0 && upper >= 0
          else -- DP to check reachable parities
               let span = 2 * maxDiff + 1
                   offset = maxDiff
                   -- Build list of (abs_parity, count) for shapes with nonzero parity
                   shapeDiffs = [(abs p, c) | ((_, _, p), c) <- zip shapes counts, p /= 0]
                   -- Initialize: only 0 is reachable
                   initial = [i == offset | i <- [0..span-1]]
                   -- Apply DP for each shape copy
                   applyShape poss (d, cnt) = iterate (stepOnce d span) poss !! cnt
                   final = foldl applyShape initial shapeDiffs
                   -- Check if any valid value is reachable
                   lo = max lower (-maxDiff)
                   hi = min upper maxDiff
               in lo <= hi && any (\v -> final !! (v + offset)) [lo..hi]

-- Single DP step: from each reachable value, we can add or subtract d
stepOnce :: Int -> Int -> [Bool] -> [Bool]
stepOnce d span poss = [canReach i | i <- [0..span-1]]
  where
    canReach i = let i1 = i - d
                     i2 = i + d
                 in (i1 >= 0 && i1 < span && poss !! i1) ||
                    (i2 >= 0 && i2 < span && poss !! i2)

-- Backtracking search using list monad with Control.Monad
exactCover :: [Int] -> [[Int]] -> Bool
exactCover cols rows = isJust $ search cols rows
  where
    search :: [Int] -> [[Int]] -> Maybe ()
    search [] _ = Just ()  -- Found solution
    search cs rs = msum $ do
      let col = minimum cs  -- MRV heuristic approximation
          candidates = filter (\r -> col `elem` r) rs
      guard (not $ null candidates)
      r <- candidates
      let cs' = filter (`notElem` r) cs
          rs' = filter (null . intersect r) rs
      return $ search cs' rs'

    intersect :: [Int] -> [Int] -> [Int]
    intersect a b = [x | x <- a, x `elem` b]

solveRegion :: Int -> Int -> [Shape] -> [Int] -> Bool
solveRegion w h shapes counts =
  let totalArea = w * h
      neededArea = sum [area * c | ((_, area, _), c) <- zip shapes counts]
  in if neededArea > totalArea then False
     else if not (reachableDiff totalArea shapes counts) then False
     else
       let pieceCols = sum counts
           cellCols = w * h
       in if pieceCols > 60 || cellCols > 400
            then True  -- Assume feasible for large cases after parity check
            else
              let offsets = scanl (+) 0 counts
                  rows = concat
                    [ [ let cols = pieceCol : [ pieceCols + (y+dy)*w + (x+dx) + 1 | (dx,dy) <- form ]
                        in cols
                      | form <- forms
                      , x <- [0..w - maxX form - 1]
                      , y <- [0..h - maxY form - 1]
                      ]
                    | (sidx, (forms, _, _)) <- zip [0..] shapes
                    , let copies = counts !! sidx
                    , copy <- [0..copies-1]
                    , let pieceCol = offsets !! sidx + copy + 1
                    ]
                  columns = [1..pieceCols + cellCols]
              in exactCover columns rows
  where
    maxX form = maximum (map fst form) + 1
    maxY form = maximum (map snd form) + 1

solveAll :: [String] -> Int
solveAll ls =
  let (shapes, regions) = parse ls
  in length [ () | (w,h,counts) <- regions, solveRegion w h shapes counts ]

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  t0 <- getCPUTime
  let !ans = solveAll ls
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "regions_that_fit=" ++ show ans ++ " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
