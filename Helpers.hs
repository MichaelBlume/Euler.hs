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

import Control.Applicative
import Primes
import Onelines

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

allProds a b = (*) <$> a <*> b

nonDetProduct = foldr allProds [1]

getPows (n,b) = map (pow b) [0..n]

allDivs = nonDetProduct . (map getPows) . encodeDirect . primeFactorization

sumProDivs = (myData !!) where
myData = map helper [0..]
helper 0 = 0
helper n = (sum $ allDivs n) - n

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
  helper (fp:rp) n
    | fp * fp > n = [n]
    | divs n fp = fp:(helper (fp:rp) (div n fp))
    | otherwise = helper rp n

pack :: (Eq x) => [x] -> [[x]]
pack l = packHelp l [] where
    packHelp [] l = [l]
    packHelp (next:rst) [] = packHelp rst [next]
    packHelp (next:rst) (last:more)
        | next == last = packHelp rst (last:last:more)
        | otherwise = ((last:more):packHelp rst [next])

encodeDirect :: (Eq x, Integral y) => [x] -> [(y, x)]
encodeDirect [] = []
encodeDirect (first:rst) = encodeDirectHelper (1, first) rst where 
    encodeDirectHelper (count, current) [] = [(count, current)]
    encodeDirectHelper (count, current) (first:rst)
        | current == first = 
            encodeDirectHelper (count + 1, current) rst
        | otherwise = 
            (count, current):(encodeDirectHelper (1, first) rst)
