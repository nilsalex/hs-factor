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

prop_factor_large :: RandomGen g => g -> Gen Bool
prop_factor_large g = do
  p <- choose (1000, 10000) :: Gen Integer
  q <- choose (1000, 10000) :: Gen Integer
  let n = p*q
  let k = factor g n
  return $ k > 1 && n `mod` k == 0 && k * (n `div` k) == n

main :: IO ()
main = do
    g <- getStdGen
    quickCheck (withMaxSuccess 100000 (prop_factor g))
    quickCheck (withMaxSuccess 1000 (prop_factor g))
