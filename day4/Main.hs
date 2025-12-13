{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed
import Data.Char (isSpace)
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseGrid :: [String] -> (UArray (Int,Int) Char, Int, Int)
parseGrid ls =
  let h = length ls
      w = maximum (map length ls)
      arr = array ((0,0),(h-1,w-1))
            [((r,c), if c < length (ls !! r) then (ls !! r) !! c else ' ')
            | r <- [0..h-1], c <- [0..w-1]]
  in (arr,h,w)

neighbors :: [(Int,Int)]
neighbors = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

part1 :: UArray (Int,Int) Char -> Int -> Int -> Int
part1 grid h w =
  length
  [ ()
  | r <- [0..h-1], c <- [0..w-1]
  , grid!(r,c) == '@'
  , let cnt = length [ ()
                     | (dr,dc) <- neighbors
                     , let r' = r+dr
                     , let c' = c+dc
                     , r' >= 0 && r' < h && c' >= 0 && c' < w
                     , grid!(r',c') == '@'
                     ]
  , cnt < 4
  ]

part2 :: UArray (Int,Int) Char -> Int -> Int -> Int
part2 grid h w =
  let arr = grid
      counts = accumArray (+) 0 ((0,0),(h-1,w-1))
               [((r,c),1) | r <- [0..h-1], c <- [0..w-1], arr!(r,c)=='@']
      mutable = part2remove arr counts
  in mutable

part2remove :: UArray (Int,Int) Char -> UArray (Int,Int) Int -> Int
part2remove grid counts = go grid counts 0
  where
    ((r0,c0),(rh,ch)) = bounds grid
    go arr cnts !acc =
      let removable = [ (r,c)
                      | r <- [r0..rh], c <- [c0..ch]
                      , arr!(r,c)=='@'
                      , let neighborsCnt = length [ ()
                                                  | (dr,dc) <- neighbors
                                                  , let r' = r+dr
                                                  , let c' = c+dc
                                                  , inBounds r' c'
                                                  , arr!(r',c')=='@'
                                                  ]
                      , neighborsCnt < 4
                      ]
      in if null removable then acc
         else
           let arr' = arr // [((r,c),'x') | (r,c) <- removable]
               acc' = acc + length removable
           in go arr' cnts acc'
    inBounds r c = r>=r0 && r<=rh && c>=c0 && c<=ch

main :: IO ()
main = do
  ls <- lines <$> readFile "inputl.txt"
  let (grid,h,w) = parseGrid ls
  t0 <- getCPUTime
  let p1 = part1 grid h w
  let p2 = part2 grid h w
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "accessible=" ++ show p1 ++ " removable_total=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
