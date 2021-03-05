module Roots
    ( root
    , findRoot
    ) where

findRoot :: Integer -> Maybe Integer
findRoot n = go lower
  where
    lower = 2
    upper = ceiling $ logBase 3 (fromIntegral n)

    go k
      | k == upper = Nothing
      | otherwise  =
          case root k n of
              Nothing -> go (k+1)
              Just r  -> Just r

root :: Integer -> Integer -> Maybe Integer
root k n = go lower upper mid
  where
    numerical = fromIntegral n ** (1 / fromIntegral k)

    lower = floor (0.99 * numerical)
    upper = ceiling (1.01 * numerical)
    mid = (lower + upper) `div` 2

    go l u m = case guess `compare` n of
        LT -> if l == u || l == m
              then Nothing
              else go m u ((m + u) `div` 2)
        EQ -> Just m
        GT -> if l == u || u == m
              then Nothing
              else go l m ((l + m) `div` 2)
      where
        guess = m^k
