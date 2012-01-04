module Helpers
( encodeDirect
, primeFactorization
, split
, pow
, sumDigs
, sumProDivs
, fibs
, maximizeFunc
) where

import Control.Applicative ((<$>), (<*>))

import Primes (primes)
import Onelines (divs)

maximizeFunc :: (Ord b) => (a -> b) -> [a] -> a
maximizeFunc f [] = error "empty list"
maximizeFunc f (n:ns) = helper n (f n) ns where
  helper b fb [] = b
  helper b fb (n:ns)
    | fb > fn = helper b fb ns
    | otherwise = helper n fn ns where
        fn = f n

fibs :: (Integral n) => [n]
fibs = 1:1: (zipWith (+) (tail fibs) fibs)


sumProDivs = (myData !!) where
  myData = map helper [0..]
  helper 0 = 0
  helper n = (sum $ allDivs n) - n

  allDivs = nonDetProduct . (map getPows) . encodeDirect . primeFactorization where
    nonDetProduct = foldr allProds [1]
    allProds a b = (*) <$> a <*> b
    getPows (n,b) = map (pow b) [0..n]

sumDigs :: (Integral i) => i -> Int
sumDigs = sum . map (\n -> read [n]) . show

pow _ 0 = 1
pow a b
  | divs b 2 = pow (a*a) (div b 2)
  | otherwise = a * (pow a (b-1))

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split s (h:t)
  | s == h = []:(split s t)
  | otherwise = (h:first):rest where
      (first:rest) = split s t

primeFactorization :: Int -> [Int]
primeFactorization = helper primes where
  helper _ 1 = []
  helper ps@(fp:rp) n
    | fp * fp > n = [n]
    | divs n fp = fp:(helper ps (div n fp))
    | otherwise = helper rp n

encodeDirect :: (Eq x, Integral y) => [x] -> [(y, x)]
encodeDirect = foldr stepper [] where
  stepper x [] = [(1,x)]
  stepper x ps@((n,el):rp)
    | x == el = (n+1,x):rp
    | otherwise = (1,x):ps
