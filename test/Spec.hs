import Test.QuickCheck

import Factor
    ( factor
    )

prop_factor :: Int -> Bool
prop_factor n = n `mod` k == 0 && k * (n `div` k) == n
  where
    k = factor n

main :: IO ()
main = quickCheck prop_factor

