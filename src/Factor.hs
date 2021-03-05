{-# LANGUAGE BangPatterns #-}

module Factor
    ( factor
    , factorShor
    , periodNaive
    , modExp
    ) where

import Roots
    ( findRoot
    )

import System.Random
    ( RandomGen
    , randomR
    )

-- | Given a non-zero integer, returns a non-trivial factor if the number is
-- composite. Otherwise, 1 is returned.
--
-- Does not work for primes yet.
factor :: RandomGen g => g -> Integer -> Integer
factor g n
    | n == 1    = 1
    | even n    = 2
    | otherwise =
        case findRoot n of
            Just r  -> r
            Nothing -> factorShor g n

-- | Shor's algorithm
--
-- Given a number which is the product of two relatively prime integers,
-- yields a non-trivial factor of this composite number.
factorShor :: RandomGen g => g -> Integer -> Integer
factorShor g n
    | gcDiv > 1 = gcDiv       -- we were lucky!
    | odd r     = factor g' n -- try another guess
    | b == n-1  = factor g' n -- try another guess
    | otherwise = gcd (b+1) n -- we found a solution!
  where
    (guess, g') = randomR (2,n-1) g
    gcDiv       = gcd n guess
    r           = periodNaive n guess
    b           = modExp n guess (r `div` 2)

-- | the period \( r \) of an element \( a \in (\mathbb{Z}/n\mathbb{Z})^\times \)
--
-- @
-- modExp n a (periodNaive n a) = 1
-- @
--
-- classically and naive, i.e. very slow
periodNaive :: Integer -> Integer -> Integer
periodNaive order base = go 1 base
  where
    go r acc
      | acc == 1  = r
      | otherwise = let !acc' = acc * base `mod` order
                        !r'   = r + 1
                    in go r' acc'

-- | modular exponentiation by squaring
--
-- @
-- modExp n b e = b^e ``mod`` n
-- @
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
