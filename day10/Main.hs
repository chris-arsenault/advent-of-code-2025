{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Bits
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import System.CPUTime (getCPUTime)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseBetween :: Char -> Char -> String -> Maybe String
parseBetween a b s =
  case (elemIndex a s, elemIndexR b s) of
    (Just i, Just j) | j > i -> Just (take (j - i - 1) (drop (i+1) s))
    _ -> Nothing
  where
    elemIndex c = lookupIndex 0 s
    lookupIndex _ [] = Nothing
    lookupIndex n (x:xs) = if x==c then Just n else lookupIndex (n+1) xs
    elemIndexR c = let idx = lookupR (length s - 1) (reverse s)
                   in case idx of
                        Nothing -> Nothing
                        Just k -> Just (k)
      where
        lookupR _ [] = Nothing
        lookupR n (x:xs) = if x==c then Just n else lookupR (n-1) xs

splitNums :: String -> [Int]
splitNums t = [ read (trim part) | part <- splitOn ',' t, not (null (trim part)) ]

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
  | x == c = "":rest
  | otherwise = (x:head rest):tail rest
  where rest = splitOn c xs

parseButtons :: String -> [[Int]]
parseButtons line = go line
  where
    go [] = []
    go s =
      case (elemIndex '(' s, elemIndex ')' s) of
        (Just l, Just r) | r > l -> splitNums (take (r-l-1) (drop (l+1) s)) : go (drop (r+1) s)
        _ -> []
    elemIndex c = lookupIndex 0
      where lookupIndex _ [] = Nothing
            lookupIndex n (x:xs) | x == c = Just n
                                 | otherwise = lookupIndex (n+1) xs

-- Part A GF(2)
rrefBinary :: [[Bool]] -> [Bool] -> Int -> Int -> ([[Bool]],[Bool],[Int],Int)
rrefBinary mat tgt lights buttons = go 0 0 mat tgt (replicate lights (-1))
  where
    go !row !col m t pivots
      | row >= lights || col >= buttons = (m,t,pivots,row)
      | otherwise =
          case findPivot row col m of
            Nothing -> go row (col+1) m t pivots
            Just r ->
              let m1 = swap row r m
                  t1 = swapT row r t
                  m2 = eliminateBelow row col m1
                  t2 = eliminateT row col m1 t1
                  pivots' = take row pivots ++ [col] ++ drop (row+1) pivots
              in go (row+1) (col+1) m2 t2 pivots'
    findPivot r c mtx = findIndexFrom r (\i -> mtx !! i !! c) mtx
    findIndexFrom i p mtx =
      if i >= length mtx then Nothing
      else if p i then Just i else findIndexFrom (i+1) p mtx
    swap i j xs =
      let xi = xs !! i
          xj = xs !! j
      in set i xj (set j xi xs)
    swapT i j xs =
      let xi = xs !! i
          xj = xs !! j
      in set i xj (set j xi xs)
    eliminateBelow r c mtx =
      [ if i <= r then mtx !! i
        else if mtx !! i !! c
             then zipWith xorRow (mtx !! i) (mtx !! r)
             else mtx !! i
      | i <- [0..lights-1] ]
    eliminateT r c mtx tg =
      [ if i <= r then tg !! i
        else if mtx !! i !! c then tg !! i /= tg !! r else tg !! i
      | i <- [0..lights-1] ]
    xorRow a b = a /= b
    set i v xs = take i xs ++ [v] ++ drop (i+1) xs

backSub :: [[Bool]] -> [Bool] -> [Int] -> Int -> [Bool]
backSub m t pivots rank = go (rank-1) (replicate (length (head m)) False)
  where
    go (-1) sol = sol
    go i sol =
      let col = pivots !! i
          val = foldl' (\acc c -> if c == col then acc else if m !! i !! c then acc /= sol !! c else acc) (t !! i) [0..length sol -1]
      in go (i-1) (set col val sol)
    set idx v xs = take idx xs ++ [v] ++ drop (idx+1) xs

solveMinPress :: [[Bool]] -> [Bool] -> [Int] -> Int -> Int
solveMinPress mat tgt pivots rank =
  let buttons = length (head mat)
      pivotSet = [ c | c <- pivots, c >= 0 ]
      freeCols = [ c | c <- [0..buttons-1], not (c `elem` pivotSet) ]
      freeCount = length freeCols
  in if freeCount > 20
       then let sol = backSub mat tgt pivots rank
            in length (filter id sol)
       else let combos = 1 `shiftL` freeCount
            in minimum [ weight sol
                       | mask <- [0..combos-1]
                       , let sol = assign mask
                       ]
  where
    buttons = length (head mat)
    assign mask =
      let sol0 = replicate buttons False
          sol1 = foldl' (\s (k,col) -> if testBit mask k then set col True s else s) sol0 (zip [0..] freeCols)
          sol2 = backfill sol1 (rank-1)
      in sol2
    backfill sol i
      | i < 0 = sol
      | otherwise =
          let col = pivots !! i
              val = foldl' (\acc c -> if c == col then acc else if mat !! i !! c then acc /= sol !! c else acc) (tgt !! i) [0..buttons-1]
              sol' = set col val sol
          in backfill sol' (i-1)
    set idx v xs = take idx xs ++ [v] ++ drop (idx+1) xs
    weight sol = length (filter id sol)

part1 :: [String] -> Int
part1 linesIn = sum (map solveLine linesIn)
  where
    solveLine line =
      case parseBetween '[' ']' line of
        Nothing -> 0
        Just pat ->
          let lights = length pat
              tgt = map (=='#') pat ++ replicate (1024 - lights) False
              buttons = parseButtons line
              bcount = length buttons
              mat = [ [ toggle btn c | btn <- buttons ] | c <- [0..lights-1] ]
              toggle btn c = c `elem` btn
              (m2,t2,pivs,rank) = rrefBinary mat (take lights tgt) lights bcount
          in solveMinPress m2 t2 pivs rank

-- Part 2 brute-force bounded search
part2 :: [String] -> Int
part2 = sum . map solveLine
  where
    solveLine line =
      case parseBetween '{' '}' line of
        Nothing -> 0
        Just nums ->
          let targets = splitNums nums
              cnt = length targets
              buttons = parseButtons line
              cap = 12
              best = search buttons targets cap
          in best
    search buttons targets cap = go 0 (replicate (length buttons) 0) (maxBound :: Int)
      where
        go idx assigns best
          | idx == length buttons =
              let totals = foldl' (\acc (btn,v) -> if v==0 then acc else addCounts acc btn v) (replicate (length targets) 0) (zip buttons assigns)
              in if totals == targets then min best (sum assigns) else best
          | otherwise =
              let best' = foldl' (\b v -> if sum assigns + v >= b then b else go (idx+1) (set idx v assigns) b) best [0..cap]
              in best'
        addCounts acc btn v = foldl' (\a i -> if i < length a then set i (a!!i + v) a else a) acc btn
        set i v xs = take i xs ++ [v] ++ drop (i+1) xs

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  t0 <- getCPUTime
  let p1 = part1 ls
      p2 = part2 ls
  t1 <- getCPUTime
  let elapsed = fromIntegral (t1 - t0) / 1e9 :: Double
  putStrLn $ "min_lights_presses=" ++ show p1 ++ " min_counter_presses=" ++ show p2 ++
             " elapsed_ms=" ++ showFF elapsed

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 1000)) / 1000 :: Double)
           in if '.' `elem` s then s else s ++ ".0"
