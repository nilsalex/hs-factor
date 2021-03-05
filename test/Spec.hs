import Test.QuickCheck

import Factor
    ( factor
    )

import System.Random
    ( getStdGen
    , RandomGen
    )

prop_factor :: RandomGen g => g -> Positive Integer -> Positive Integer -> Bool
prop_factor g (Positive p) (Positive q) = k > 1 && n `mod` k == 0 && k * (n `div` k) == n
  where
    n = (p+1) * (q+1) -- composite numbers start at (1+1) * (1+1) = 4
    k = factor g n

main :: IO ()
main = do
    g <- getStdGen
    quickCheck (withMaxSuccess 1000000 (prop_factor g))
