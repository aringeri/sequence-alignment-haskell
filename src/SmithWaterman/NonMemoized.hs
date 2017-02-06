module SmithWaterman.NonMemoized (
  align
) where

import Data.Ord (comparing)
import Data.Foldable (maximumBy)

align :: String -> String -> (Int, String, String)
align = alignSeq '-'

padRest a = replicate (length a)

--Recursive, non-memoized smith waterman
alignSeq :: Eq s => s -> [s] -> [s] -> (Int, [s], [s])
alignSeq gap [] ys = (-(length ys), padRest ys gap, ys)
alignSeq gap xs [] = (-(length xs), xs, padRest xs gap)
alignSeq gap allX@(x:xs) allY@(y:ys) = maxScore [takeX, skipX, skipY]
         where takeX = nextWith gap x   xs   y   ys
               skipX = nextWith gap gap allX y   ys
               skipY = nextWith gap x   xs   gap allY  

maxScore :: Eq s => [(Int, [s], [s])] -> (Int, [s], [s]) 
maxScore = maximumBy (comparing fst3)
         where fst3 (a,_,_) = a

nextWith gap x xs y ys = (acc + score x y, x:accX, y:accY)
         where (acc, accX, accY) = alignSeq gap xs ys

score x y | x == y    = 2
          | otherwise = -1
