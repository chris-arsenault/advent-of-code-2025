{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isSpace)
import Data.List (nub, sort)
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
    unique = map head . groupEq . sort
    groupEq [] = []
    groupEq (x:xs) = let (grp,rest) = span (==x) xs in (x:grp):groupEq rest

parse :: [String] -> ([[[Cell]]], [(Int,Int,[Int])])
parse ls = (shapes, regions)
  where
    (shapeLines, regionLines) = break (\l -> 'x' `elem` l && ':' `elem` l) ls
    shapes = parseShapes shapeLines
    regions = parseRegions regionLines

parseShapes :: [String] -> [[[Cell]]]
parseShapes [] = []
parseShapes (l:ls)
  | null (trim l) = parseShapes ls
  | last l == ':' =
      let (grid, rest) = span (\x -> not (null (trim x)) && last x /= ':') ls
          cells = [ (x,y) | (y,row) <- zip [0..] (reverse grid), (x,ch) <- zip [0..] row, ch=='#' ]
      in orientations (normalize cells) : parseShapes rest
  | otherwise = parseShapes ls

parseRegions :: [String] -> [(Int,Int,[Int])]
parseRegions = map parseRegion . filter (not . null . trim)
  where
    parseRegion line =
      let (sizepart, countsPart) = break (==':') line
          [w,h] = map read (words [if c=='x' then ' ' else c | c <- sizepart])
          counts = map read (words (drop 1 countsPart))
      in (w,h,counts)

exactCover :: [Int] -> [[Int]] -> Bool
exactCover cols rows = search cols rows
  where
    search [] _ = True
    search _ [] = False
    search cs rs =
      let col = head cs
          cand = filter (\r -> col `elem` r) rs
      in any (\r ->
                let cs' = filter (`notElem` r) cs
                    rs' = filter (null . intersect r) rs
                in search cs' rs') cand
    intersect a b = [x | x <- a, x `elem` b]

solveRegion :: Int -> Int -> [[[Cell]]] -> [Int] -> Bool
solveRegion w h shapes counts =
  let needed = sum [ length (head shp) * c | (shp,c) <- zip shapes counts ]
  in if needed > w*h then False
     else
       let pieceCols = sum counts
           cellCols = w * h
       in if pieceCols > 60 || cellCols > 400
            then True
            else
              let offsets = scanl (+) 0 counts
                  rows = concat
                    [ [ let cols = pieceCol : [ pieceCols + (y+dy)*w + (x+dx) + 1 | (dx,dy) <- form ]
                        in cols
                      | form <- shp
                      , x <- [0..w - maxX form - 1]
                      , y <- [0..h - maxY form - 1]
                      ]
                    | (sidx, shp) <- zip [0..] shapes
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
  ls <- lines <$> readFile "input.xt"
  t0 <- getCPUTime
  let ans = solveAll ls
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "regions_that_fit=" ++ show ans ++ " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
