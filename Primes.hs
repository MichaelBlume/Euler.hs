module Primes
( primes
, isPrime
) where

import Onelines


isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime x
  | x < 2 = False
  | otherwise = not $ any (divs x) $ primesBelowSqrt x

primesBelowSqrt :: (Integral a) => a -> [a]
primesBelowSqrt square = pbHelp primeMap where
    pbHelp ((num, prime):rst)
        | num * num > square = []
        | not prime = pbHelp rst
        | otherwise = num:(pbHelp rst)

primeMap :: (Integral a) => [(a, Bool)]
primeMap = map (\x -> (x, isPrime x)) [2..]

primes :: (Integral a) => [a]
primes = map fst $ filter snd $ primeMap

