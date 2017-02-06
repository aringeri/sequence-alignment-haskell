module SmithWaterman.Memoized (
  align
)
where

import Data.Array
import Data.List
import Data.Ord

data Action = Both | X | Y 
    deriving (Show, Eq, Ord)

align :: String -> String -> (Int, (String, String))
align s1 s2 = (topScore, traceBy maxIx ("","")) where
                table = buildTable s1 s2
                topScore = fst $ table!maxIx
                maxIx = maximumBy (comparing (table!)) (indices table) 
                traceBy :: (Int, Int) -> (String, String) -> (String, String)
                traceBy (x,0) (accX,accY) = (take x s1 ++ accX, replicate x '-' ++ accY)
                traceBy (0,y) (accX,accY) = (replicate y '-' ++ accX, take y s2 ++ accY)
                traceBy p@(x,y) (accX, accY) = (as, bs)
                  where (_,v) = table!p
                        a     = case v of
                                  Both -> s1!!(x-1)
                                  X    -> s1!!(x-1)
                                  Y    -> '-'
                        b     = case v of 
                                  Both -> s2!!(y-1)
                                  X    -> '-'
                                  Y    -> s2!!(y-1)
                        (as,bs) = case v of
                                  Both -> traceBy (x-1, y-1) (a:accX, b:accY)
                                  X    -> traceBy (x-1, y)   (a:accX, '-':accY)
                                  Y    -> traceBy (x,   y-1) ('-':accX, b:accY)
		 

buildTable :: String -> String -> Array (Int,Int) (Int, Action)
buildTable s1 s2 = a where
                   n = length s1
                   m = length s2
                   a = array ( (0,0), (n,m) )
                       ( [((0,y), (0,X)) | y <- [0..m]] ++ --Initialize first row
                         [((x,0), (0,Y)) | x <- [1..n]] ++ --Initialize first column
                         [((x,y), maxScore x y) | x <- [1..n], y <- [1..m]] )
                   maxScore x y = bestOf [takeXY, takeX, takeY]
                     where
                        takeXY = (fst (a!(x-1,y-1)) + cmp(s1!!(x-1)) (s2!!(y-1)), Both)
                        takeX  = (fst (a!(x-1,y)) + gapPenalty, X)
                        takeY  = (fst (a!(x,y-1)) + gapPenalty, Y)

cmp a b | a == b = 2
        | otherwise = -1
gapPenalty = -1

bestOf = maximumBy $ comparing fst

printArray :: Show a => Array (Int, Int) a -> String
printArray arr =
  unlines [unwords [show (arr ! (x, y)) | x <- [minX..maxX]] | y <- [minY..maxY]]
    where ((minX, minY), (maxX, maxY)) = bounds arr
