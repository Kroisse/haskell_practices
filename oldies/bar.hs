import Data.List
import Data.Maybe (fromMaybe)

shortestComposableLength seq = fromMaybe (-1) $ find (isComposable histo) [m..limit]
  where histo = histogram seq
        m = fst $ head histo
        limit = sum seq - 1
    
isComposable histo n = case grab histo n of
  Just [] -> True
  Nothing -> False
  Just a  -> isComposable a n
  
grab :: Integral a => [(a, Int)] -> a -> Maybe [(a, Int)]
grab seq 0 = Just seq
grab seq n = let (big,other) = span ((>n) . fst) seq
             in case other of
               [] -> Nothing
               ((i,k):small) ->
                 let remain = big ++ (if k <= 1
                                      then small
                                      else (i,k-1):small)
                 in grab remain (n-i)

histogram :: Ord a => [a] -> [(a, Int)]
histogram [] = []
histogram seq = histo' x 1 xs
  where (x:xs) = sortBy (flip compare) seq
        histo' x n [] = [(x,n)]
        histo' x n (a:as)
          | x == a    = histo' x (n+1) as
          | otherwise = (x,n) : histo' a 1 as


testData = [ ([9,9,7,7,4,4,4,4,4,3,2,1,1,1], 12)
           , ([5,2,1,5,2,1,5,2,1], 6)
           , ([1,1,1,1,1,1,1], 1)
           , ([2,2,1,3,3,5,4], 5)
           , ([1,2,3,4], 5)
           , ([5,4,3,2,1], 5)
           , ([3,2,4,5,2], 8)
           , ([5,3,2,4,4,2], 10)
           , ([7,4,3,2,1], -1)
           , ([3,3,3,2,3,3,3,3,2,3], 14)
           ]

-- HUnit이 있긴 한데 일단 대충 때움
main = print $ filter (test shortestComposableLength) testData
  where test f (val,expected) = not (f val == expected)