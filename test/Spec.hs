import Test.QuickCheck

import Factor
    ( factor
    )

prop_factor :: Positive Int -> Positive Int -> Bool
prop_factor (Positive p) (Positive q) = k > 1 && n `mod` k == 0 && k * (n `div` k) == n
  where
    n = (p+1) * (q+1) -- composite numbers start at (1+1) * (1+1) = 4
    k = factor n

main :: IO ()
main = quickCheck (withMaxSuccess 1000000 prop_factor)

