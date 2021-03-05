{-# LANGUAGE BangPatterns #-}

module Factor
    ( factor
    ) where

import System.Random

factor :: RandomGen g => g -> Integer -> Integer
factor g n
  | n == 1    = 1
  | even n    = 2
  | otherwise = factorShor g n

-- assumption
-- ∃ p,q relatively prime : n = p*q
factorShor :: RandomGen g => g -> Integer -> Integer
factorShor g n
    | gcDiv > 1 = gcDiv
    | odd r     = factorShor g' n -- try again!
    | b == -1   = factorShor g' n -- try again!
    | otherwise = gcd (b+1) n
  where
    (guess, g') = randomR (2,n-1) g
    gcDiv       = gcd n guess
    r           = findPeriodNaive n guess
    b           = modExp n guess (r `div` 2)

findPeriodNaive :: Integer -> Integer -> Integer
findPeriodNaive order base = go 1 base
  where
    go r acc
      | acc == 1  = r
      | otherwise = let !acc' = acc * base `mod` order
                        !r'   = r + 1
                    in go r' acc'

modExp :: Integer -> Integer -> Integer -> Integer
modExp n b = go b 1
  where
    go acc1 acc2 e
        | e == 0    = 1
        | e == 1    = acc1 * acc2 `mod` n
        | even e    = let !acc1' = acc1 * acc1 `mod` n
                          !e'    = e `div` 2
                      in go acc1' acc2 e'
        | otherwise = let !acc2' = acc1 * acc2 `mod` n
                          !e'    = e - 1
                      in go acc1 acc2' e'
