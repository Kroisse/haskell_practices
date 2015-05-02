seq3n1 :: (Eq a, Integral a) => a -> [a]
seq3n1 1 = [1]
seq3n1 n = n : seq3n1 next
  where next = if even n
               then n `div` 2
               else 3*n + 1

main :: IO ()
main =
  do a <- readLn :: IO Integer
     b <- readLn
     print . maximum . map (length . seq3n1) $ [a..b]
