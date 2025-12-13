{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Bits
import Data.List (foldl')
import Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad (forM_, when)
import Control.Monad.ST
import System.CPUTime (getCPUTime)

-- Parsing utilities
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
  | x == c = "" : rest
  | otherwise = (x : head rest) : tail rest
  where rest = splitOn c xs

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

parseBetween :: Char -> Char -> String -> Maybe String
parseBetween a b s = go1 s 0
  where
    go1 [] _ = Nothing
    go1 (x:xs) i
      | x == a = go2 xs (i+1) (i+1)
      | otherwise = go1 xs (i+1)
    go2 [] _ _ = Nothing
    go2 (x:xs) start i
      | x == b = Just (take (i - start) (drop start s))
      | otherwise = go2 xs start (i+1)

splitNums :: String -> [Int]
splitNums t = [read (trim p) | p <- splitOn ',' t, not (null (trim p))]

parseButtons :: String -> [[Int]]
parseButtons = go
  where
    go [] = []
    go s = case (findIdx '(' s 0, findIdx ')' s 0) of
      (Just l, Just r) | r > l ->
        splitNums (take (r-l-1) (drop (l+1) s)) : go (drop (r+1) s)
      _ -> []
    findIdx _ [] _ = Nothing
    findIdx c (x:xs) i = if x == c then Just i else findIdx c xs (i+1)

-- Part 1: GF(2) using bit-packed rows (Integer for arbitrary width)
type BitRow = Integer

setBit' :: BitRow -> Int -> BitRow
setBit' r i = r `xor` (1 `shiftL` i)

getBit :: BitRow -> Int -> Bool
getBit r i = testBit r i

xorRow :: BitRow -> BitRow -> BitRow
xorRow = xor

-- Gaussian elimination over GF(2) returning (matrix, target, pivotCols, rank)
rrefGF2 :: Int -> Int -> V.Vector BitRow -> U.Vector Bool -> (V.Vector BitRow, U.Vector Bool, U.Vector Int, Int)
rrefGF2 lights buttons mat0 tgt0 = runST $ do
    matM <- V.thaw mat0
    tgtM <- U.thaw tgt0
    pivotsM <- MU.replicate lights (-1)

    let go !row !col
          | row >= lights || col >= buttons = return row
          | otherwise = do
              -- Find pivot
              pivotRow <- findPivot matM row lights col
              case pivotRow of
                Nothing -> go row (col+1)
                Just pr -> do
                  -- Swap rows
                  when (pr /= row) $ do
                    mr <- MV.read matM row
                    mp <- MV.read matM pr
                    MV.write matM row mp
                    MV.write matM pr mr
                    tr <- MU.read tgtM row
                    tp <- MU.read tgtM pr
                    MU.write tgtM row tp
                    MU.write tgtM pr tr
                  -- Record pivot
                  MU.write pivotsM row col
                  -- Eliminate below
                  pivotRowVal <- MV.read matM row
                  forM_ [row+1..lights-1] $ \r -> do
                    rv <- MV.read matM r
                    when (getBit rv col) $ do
                      MV.write matM r (xorRow rv pivotRowVal)
                      tv <- MU.read tgtM r
                      tp <- MU.read tgtM row
                      MU.write tgtM r (tv /= tp)
                  go (row+1) (col+1)

    rank <- go 0 0

    -- Back-substitute to RREF
    forM_ [rank-1, rank-2 .. 0] $ \i -> do
      col <- MU.read pivotsM i
      when (col >= 0) $ do
        pivotRowVal <- MV.read matM i
        forM_ [0..i-1] $ \r -> do
          rv <- MV.read matM r
          when (getBit rv col) $ do
            MV.write matM r (xorRow rv pivotRowVal)
            tv <- MU.read tgtM r
            ti <- MU.read tgtM i
            MU.write tgtM r (tv /= ti)

    matF <- V.freeze matM
    tgtF <- U.freeze tgtM
    pivotsF <- U.freeze pivotsM
    return (matF, tgtF, pivotsF, rank)
  where
    findPivot matM row lights col = go' row
      where
        go' r
          | r >= lights = return Nothing
          | otherwise = do
              rv <- MV.read matM r
              if getBit rv col then return (Just r) else go' (r+1)

solveMinPressGF2 :: Int -> V.Vector BitRow -> U.Vector Bool -> U.Vector Int -> Int -> Int
solveMinPressGF2 buttons mat tgt pivots rank =
  let pivotSet = U.toList (U.take rank pivots)
      freeCols = [c | c <- [0..buttons-1], c `notElem` pivotSet]
      freeCount = length freeCols
  in if freeCount > 20
       then -- Just compute one solution
         let sol = backSub (replicate buttons False)
         in length (filter id sol)
       else -- Enumerate all 2^freeCount combinations
         minimum [weight (assign mask freeCols) | mask <- [0..(1 `shiftL` freeCount)-1]]
  where
    backSub sol0 = foldl' step sol0 [rank-1, rank-2 .. 0]
      where
        step sol i =
          let col = pivots U.! i
              val = foldl' (\acc c -> if c == col then acc
                                      else if getBit (mat V.! i) c
                                           then acc /= (sol !! c)
                                           else acc)
                           (tgt U.! i) [0..buttons-1]
          in take col sol ++ [val] ++ drop (col+1) sol

    assign mask fcols =
      let sol0 = replicate buttons False
          sol1 = foldl' (\s (k,col) -> if testBit mask k
                                       then take col s ++ [True] ++ drop (col+1) s
                                       else s) sol0 (zip [0..] fcols)
      in backfill sol1 (rank-1)

    backfill sol i
      | i < 0 = sol
      | otherwise =
          let col = pivots U.! i
              val = foldl' (\acc c -> if c == col then acc
                                      else if getBit (mat V.! i) c
                                           then acc /= (sol !! c)
                                           else acc)
                           (tgt U.! i) [0..buttons-1]
          in backfill (take col sol ++ [val] ++ drop (col+1) sol) (i-1)

    weight sol = length (filter id sol)

part1 :: [String] -> Int
part1 = sum . map solveLine
  where
    solveLine line = case parseBetween '[' ']' line of
      Nothing -> 0
      Just pat ->
        let lights = length pat
            tgt = U.fromList [c == '#' | c <- pat]
            btns = parseButtons line
            bcount = length btns
            mat = V.fromList [buildRow lights btn | btn <- btns]
            matT = V.generate lights $ \r ->
                     foldl' (\acc (c, row) -> if getBit row r then setBit' acc c else acc) 0 (zip [0..] (V.toList mat))
            (m2, t2, pivs, rnk) = rrefGF2 lights bcount matT tgt
            -- Check consistency
            consistent = all (\r -> not (t2 U.! r)) [rnk..lights-1]
        in if not consistent then 0 else solveMinPressGF2 bcount m2 t2 pivs rnk

    buildRow :: Int -> [Int] -> BitRow
    buildRow lights btn = foldl' (\r p -> if p >= 0 && p < lights then setBit' r p else r) 0 btn

-- Part 2: Integer linear system using Rational arithmetic
type Rat = Ratio Integer

rrefRational :: Int -> Int -> V.Vector (V.Vector Rat) -> V.Vector Rat -> (V.Vector (V.Vector Rat), V.Vector Rat, U.Vector Int, Int)
rrefRational counters buttons mat0 tgt0 = runST $ do
    -- Augmented matrix [mat | tgt]
    augM <- V.thaw $ V.zipWith (\row t -> V.snoc row t) mat0 tgt0
    pivotsM <- MU.replicate counters (-1)

    let go !row !col
          | row >= counters || col >= buttons = return row
          | otherwise = do
              -- Find pivot
              pivotRow <- findPivot augM row counters col
              case pivotRow of
                Nothing -> go row (col+1)
                Just pr -> do
                  when (pr /= row) $ do
                    mr <- MV.read augM row
                    mp <- MV.read augM pr
                    MV.write augM row mp
                    MV.write augM pr mr
                  MU.write pivotsM row col
                  -- Scale pivot row
                  pivRow <- MV.read augM row
                  let piv = pivRow V.! col
                  MV.write augM row (V.map (/ piv) pivRow)
                  -- Eliminate all other rows
                  scaledPivRow <- MV.read augM row
                  forM_ [0..counters-1] $ \r -> when (r /= row) $ do
                    rv <- MV.read augM r
                    let factor = rv V.! col
                    when (factor /= 0) $
                      MV.write augM r (V.zipWith (\a b -> a - factor * b) rv scaledPivRow)
                  go (row+1) (col+1)

    rank <- go 0 0
    augF <- V.freeze augM
    pivotsF <- U.freeze pivotsM

    let matF = V.map (V.take buttons) augF
        tgtF = V.map (V.! buttons) augF

    return (matF, tgtF, pivotsF, rank)
  where
    findPivot augM row counters col = go' row Nothing 0
      where
        go' r best bestVal
          | r >= counters = return best
          | otherwise = do
              rv <- MV.read augM r
              let v = abs (rv V.! col)
              if v > 0 && v > bestVal
                then go' (r+1) (Just r) v
                else go' (r+1) best bestVal

solveMinPressInt :: Int -> V.Vector (V.Vector Rat) -> V.Vector Rat -> U.Vector Int -> Int -> Int
solveMinPressInt buttons mat rhs pivots rank =
  let pivotSet = U.toList (U.take rank pivots)
      freeCols = [c | c <- [0..buttons-1], c `notElem` pivotSet]
      freeCount = length freeCols
      -- Extract coefficients for free variables
      coefs = V.generate rank $ \r ->
                V.fromList [mat V.! r V.! fc | fc <- freeCols]
      rhsV = V.take rank rhs
      cap = max 200 (ceiling (V.maximum rhsV) + 50)
  in dfs freeCols freeCount coefs rhsV cap 0 0 (maxBound :: Int)

dfs :: [Int] -> Int -> V.Vector (V.Vector Rat) -> V.Vector Rat -> Int -> Int -> Int -> Int -> Int
dfs freeCols freeCount coefs rhsV cap fidx currentSum best
  | currentSum >= best = best
  | fidx >= freeCount =
      let totalPress = go 0 currentSum
      in if totalPress < best then totalPress else best
  | otherwise =
      let maxV = min cap (best - currentSum - 1)
      in foldl' (\b v -> dfs freeCols freeCount coefs rhsV cap (fidx+1) (currentSum+v) b) best [0..maxV]
  where
    freeVals = replicate freeCount 0  -- Placeholder, we accumulate differently

    go r acc
      | r >= V.length rhsV = acc
      | acc >= best = best
      | otherwise =
          let v = (rhsV V.! r) - sum [coefs V.! r V.! f * fromIntegral (getFreeVal f) | f <- [0..freeCount-1]]
          in if v < 0 then best
             else if denominator v /= 1 then best
             else let iv = fromIntegral (numerator v)
                  in go (r+1) (acc + iv)

    -- This is tricky - we need to pass free values through recursion
    getFreeVal f = 0  -- Placeholder - need proper implementation

-- Simpler recursive approach
solveMinPressInt2 :: Int -> Int -> V.Vector (V.Vector Rat) -> V.Vector Rat -> U.Vector Int -> Int -> Int
solveMinPressInt2 buttons rank mat rhs pivots cap =
  let pivotSet = U.toList (U.take rank pivots)
      freeCols = [c | c <- [0..buttons-1], c `notElem` pivotSet]
      freeCount = length freeCols
      coefs = [[mat V.! r V.! fc | fc <- freeCols] | r <- [0..rank-1]]
      rhsL = [rhs V.! r | r <- [0..rank-1]]
  in search freeCols freeCount coefs rhsL cap 0 (replicate freeCount 0) (maxBound :: Int)

search :: [Int] -> Int -> [[Rat]] -> [Rat] -> Int -> Int -> [Int] -> Int -> Int
search freeCols freeCount coefs rhsL cap fidx freeVals best
  | currentSum >= best = best
  | fidx >= freeCount = checkSolution coefs rhsL freeVals currentSum best
  | otherwise =
      let maxV = min cap (best - currentSum - 1)
      in foldl' (\b v -> search freeCols freeCount coefs rhsL cap (fidx+1)
                         (take fidx freeVals ++ [v] ++ drop (fidx+1) freeVals) b) best [0..maxV]
  where
    currentSum = sum (take fidx freeVals)

checkSolution :: [[Rat]] -> [Rat] -> [Int] -> Int -> Int -> Int
checkSolution coefs rhsL freeVals currentSum best = go 0 currentSum
  where
    go r acc
      | r >= length rhsL = if acc < best then acc else best
      | acc >= best = best
      | otherwise =
          let v = (rhsL !! r) - sum (zipWith (\c fv -> c * fromIntegral fv) (coefs !! r) freeVals)
          in if v < 0 then best
             else if denominator v /= 1 then best
             else let iv = fromIntegral (numerator v)
                  in go (r+1) (acc + iv)

part2 :: [String] -> Int
part2 = sum . map solveLine
  where
    solveLine line = case parseBetween '{' '}' line of
      Nothing -> 0
      Just nums ->
        let targets = map fromIntegral (splitNums nums) :: [Rat]
            btns = parseButtons line
            bcount = length btns
            counters = length targets
            mat = V.fromList [V.fromList [if p `elem` btn && p < counters then 1 else 0 | p <- [0..bcount-1]] | btn <- btns]
            matT = V.generate counters $ \c ->
                     V.fromList [if c `elem` (btns !! b) then 1 else 0 | b <- [0..bcount-1]]
            tgt = V.fromList targets
            (m2, t2, pivs, rnk) = rrefRational counters bcount matT tgt
            -- Check consistency
            consistent = all (\r -> let rowZero = V.all (== 0) (m2 V.! r)
                                    in not rowZero || t2 V.! r == 0) [rnk..counters-1]
            cap = max 200 (ceiling (maximum targets) + 50)
        in if not consistent || bcount == 0 || counters == 0
           then 0
           else solveMinPressInt2 bcount rnk m2 t2 pivs cap

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  t0 <- getCPUTime
  let !p1 = part1 ls
      !p2 = part2 ls
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "min_lights_presses=" ++ show p1 ++ " min_counter_presses=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
