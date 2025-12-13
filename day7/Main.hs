{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.CPUTime (getCPUTime)

loadGrid :: [String] -> ([String], Int, Int)
loadGrid ls =
  let grid = ls
      sr = fromJust (findIndexElem 'S' grid)
      sc = case findIndex (=='S') (grid !! sr) of
             Just c -> c
             Nothing -> error "no S"
  in (grid, sr, sc)

findIndexElem :: Char -> [String] -> Maybe Int
findIndexElem ch = go 0
  where
    go _ [] = Nothing
    go i (x:xs) = if ch `elem` x then Just i else go (i+1) xs

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = go 0
  where
    go _ [] = Nothing
    go i (x:xs) = if p x then Just i else go (i+1) xs

part1 :: [String] -> Int -> Int -> Int
part1 grid sr sc =
  let h = length grid
      w = length (head grid)
      go r splits active
        | r >= h = splits
        | S.null active = splits
        | otherwise =
            let (next,s') = step r splits active
            in go (r+1) s' next
      step r splits act =
        S.foldl' (\(acc,spl) c ->
                    let cell = grid !! r !! c
                    in if cell == '^'
                         then let spl' = spl + 1
                                  acc' = (if c > 0 then S.insert (c-1) acc else acc)
                                  acc''= if c+1 < w then S.insert (c+1) acc' else acc'
                              in (acc'', spl')
                         else (S.insert c acc, spl)
                 ) (S.empty, splits) act
  in go sr 0 (S.singleton sc)

part2 :: [String] -> Int -> Int -> Int
part2 grid sr sc =
  let h = length grid
      w = length (head grid)
      go r active
        | r >= h = active
        | M.null active = active
        | otherwise = go (r+1) (step r active)
      step r act =
        M.foldlWithKey'
          (\acc c cnt ->
             let cell = grid !! r !! c
             in if cell == '^'
                  then let acc' = if c > 0 then M.insertWith (+) (c-1) cnt acc else acc
                           acc''= if c+1 < w then M.insertWith (+) (c+1) cnt acc' else acc'
                       in acc''
                  else M.insertWith (+) c cnt acc)
          M.empty act
      final = go sr (M.singleton sc 1)
  in sum (M.elems final)

main :: IO ()
main = do
  linesIn <- lines <$> readFile "input.txt"
  let (grid, sr, sc) = loadGrid linesIn
  t0 <- getCPUTime
  let !p1 = part1 grid sr sc
      !p2 = part2 grid sr sc
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "splits=" ++ show p1 ++ " timelines=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
