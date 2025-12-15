{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import GHC.Clock (getMonotonicTimeNSec)

type Pos = (Int, Int)

neighbors :: [Pos]
neighbors = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

parseGrid :: [String] -> S.Set Pos
parseGrid ls = S.fromList
  [ (r, c)
  | (r, line) <- zip [0..] ls
  , (c, ch) <- zip [0..] line
  , ch == '@'
  ]

neighborCount :: S.Set Pos -> Pos -> Int
neighborCount rolls (r, c) =
  length [ () | (dr, dc) <- neighbors, S.member (r+dr, c+dc) rolls ]

computeCounts :: S.Set Pos -> M.Map Pos Int
computeCounts rolls = M.fromList [(p, neighborCount rolls p) | p <- S.toList rolls]

part1 :: M.Map Pos Int -> Int
part1 counts = M.size $ M.filter (< 4) counts

-- BFS wavefront removal: O(n) instead of O(n²)
part2 :: S.Set Pos -> M.Map Pos Int -> Int
part2 rolls0 counts0 = go rolls0 counts0 initialQueue S.empty 0
  where
    initialQueue = Seq.fromList [p | (p, cnt) <- M.toList counts0, cnt < 4]

    go :: S.Set Pos -> M.Map Pos Int -> Seq Pos -> S.Set Pos -> Int -> Int
    go !rolls !counts !queue !removed !acc =
      case Seq.viewl queue of
        Seq.EmptyL -> acc
        p Seq.:< rest
          | S.member p removed -> go rolls counts rest removed acc
          | otherwise ->
              let removed' = S.insert p removed
                  (r, c) = p
                  -- Update neighbors and collect newly accessible
                  (counts', newAccessible) = foldr (updateNeighbor rolls removed') (counts, []) neighbors
                    where
                      updateNeighbor rs rm (dr, dc) (cnts, acc') =
                        let np = (r + dr, c + dc)
                        in if S.member np rs && not (S.member np rm)
                           then case M.lookup np cnts of
                                  Just cnt ->
                                    let cnt' = cnt - 1
                                        cnts' = M.insert np cnt' cnts
                                    in if cnt' < 4 && cnt >= 4
                                       then (cnts', np : acc')
                                       else (cnts', acc')
                                  Nothing -> (cnts, acc')
                           else (cnts, acc')
                  queue' = foldl (|>) rest newAccessible
              in go rolls counts' queue' removed' (acc + 1)

main :: IO ()
main = do
  t0 <- getMonotonicTimeNSec
  ls <- lines <$> readFile "input.txt"
  let rolls = parseGrid ls
      counts = computeCounts rolls
      !p1 = part1 counts
      !p2 = part2 rolls counts
  p1 `seq` p2 `seq` return ()
  t1 <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  putStrLn $ "accessible=" ++ show p1 ++ " removable_total=" ++ show p2
             ++ " elapsed_ms=" ++ showFF elapsedMs
  where
    showFF x = let s = show (fromIntegral (round (x * 1000 :: Double)) / 1000 :: Double)
               in if '.' `elem` s then s else s ++ ".0"
