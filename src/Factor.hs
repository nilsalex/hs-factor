module Factor
    ( factor
    ) where

factor :: Int -> Int
factor n
  | even n    = 2
  | otherwise = factorNaive n

factorNaive :: Int -> Int
factorNaive n = go 3 n
  where
    go k n
      | k > (n `div` 2) = 1
      | n `mod` k == 0  = k
      | otherwise       = go (k+1) n
