module Main where

import Data.Array
import Data.List (genericReplicate)
import Control.Monad (forM_)
import Text.Printf

directions :: [(Integer, Integer)]
directions = cycle [(0,1),(1,0),(0,-1),(-1,0)]

distances :: (Num a, Enum a) => a -> [a]
distances n = n':dup
  where n' = n-1
        dup = foldr (\x s -> x:x:s) [] [n',n'-1..1]

snailSeq :: Integer -> [((Integer, Integer), Integer)]
snailSeq n = zip (scanl tupleAdd (1,1) (seq' n)) [1..]
  where seq' k = concat $ zipWith genericReplicate (distances k) directions
        tupleAdd (dy,dx) (y,x) = (y+dy,x+dx)

snailMatrix :: Integer -> Array (Integer,Integer) Integer
snailMatrix k = array ((1,1), (k,k)) (snailSeq k)

showMatrix :: Integer -> Array (Integer, Integer) Integer -> IO ()
showMatrix width mat =
  let ((r_0, c_0), (r_n, c_n)) = bounds mat in
  forM_ [r_0..r_n] $ \row -> do
    forM_ [c_0..c_n] $ \col -> do
      printf "%*d " width (mat!(row,col))
    putChar '\n'

drawSnailMatrix :: Integer -> IO ()
drawSnailMatrix n =
  let n' = (fromIntegral (n * n)) :: Double
      width = 1 + (truncate $ (log n') / log 10.0)
  in
  showMatrix width (snailMatrix n)


main :: IO ()
main = do
  n <- fmap read getLine
  drawSnailMatrix n
