module Primes
( primes
, isPrime
) where

import Onelines (divs, under)
import Memoize (memoizeF)


isPrime :: (Integral a) => a -> Bool
isPrime = memoizeF helper where
  helper 2 = True
  helper x
    | x < 2 = False
    | otherwise = not $ any (divs x) $ primesBelowSqrt x

primesBelowSqrt :: (Integral a) => a -> [a]
primesBelowSqrt square = pbHelp primes where
  pbHelp (p:ps)
    | p * p > square = []
    | otherwise = p:(pbHelp ps)

primes :: (Integral a) => [a]
primes = filter isPrime [2..]

