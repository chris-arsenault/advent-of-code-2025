{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import GHC.Clock (getMonotonicTimeNSec)

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
            let (next,s') = bfs r splits (Seq.fromList (S.toList active)) S.empty S.empty
            in go (r+1) s' next
      bfs r splits queue seen next
        | Seq.null queue = (next, splits)
        | otherwise =
            let (c Seq.:< rest) = Seq.viewl queue
            in if S.member c seen
                 then bfs r splits rest seen next
                 else let seen' = S.insert c seen
                          cell = grid !! r !! c
                      in if cell == '^'
                           then let splits' = splits + 1
                                    rest' = (if c > 0 then rest Seq.|> (c-1) else rest)
                                    rest''= if c+1 < w then rest' Seq.|> (c+1) else rest'
                                in bfs r splits' rest'' seen' next
                           else bfs r splits rest seen' (S.insert c next)
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
  t0 <- getMonotonicTimeNSec
  linesIn <- lines <$> readFile "input.txt"
  let (grid, sr, sc) = loadGrid linesIn
      !p1 = part1 grid sr sc
      !p2 = part2 grid sr sc
  p1 `seq` p2 `seq` return ()
  t1 <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  putStrLn $ "splits=" ++ show p1 ++ " timelines=" ++ show p2
             ++ " elapsed_ms=" ++ showFF elapsedMs
  where
    showFF x = let s = show (fromIntegral (round (x * 1000 :: Double)) / 1000 :: Double)
               in if '.' `elem` s then s else s ++ ".0"
