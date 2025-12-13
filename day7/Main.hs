{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Maybe (fromJust)
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
      go _ splits [] = splits
      go r splits active
        | r >= h = splits
        | otherwise =
            let (next,splits') = step r splits active
            in if null next then splits' else go (r+1) splits' next
      step r splits [] = ([], splits)
      step r splits (c:cs) =
        let cell = grid !! r !! c
        in if cell == '^'
             then let splits' = splits + 1
                      left = if c > 0 then [c-1] else []
                      right = if c+1 < w then [c+1] else []
                      (nxt, sp) = step r splits' cs
                  in (left ++ right ++ nxt, sp)
             else let (nxt, sp) = step r splits cs
                  in (c:nxt, sp)
  in go sr 0 [sc]

part2 :: [String] -> Int -> Int -> Int
part2 grid sr sc =
  let h = length grid
      w = length (head grid)
      go r active
        | r >= h = active
        | otherwise =
            let next = concatMap (advance r) active
            in if null next then active else go (r+1) next
      advance r (c,count) =
        let cell = grid !! r !! c
        in if cell == '^'
             then [(c-1,count) | c>0] ++ [(c+1,count) | c+1<w]
             else [(c,count)]
      final = go sr [(sc,1)]
  in sum (map snd final)

main :: IO ()
main = do
  linesIn <- lines <$> readFile "input.txt"
  let (grid, sr, sc) = loadGrid linesIn
  t0 <- getCPUTime
  let p1 = part1 grid sr sc
      p2 = part2 grid sr sc
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "splits=" ++ show p1 ++ " timelines=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
