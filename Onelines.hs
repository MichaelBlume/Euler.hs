module Onelines
( first
, second
, square
, divs
, under
) where

first (a,b) = a
second (a,b) = b

square x = x*x

divs :: (Integral a) => a -> a -> Bool
divs a b = (mod a b) == 0

under x = takeWhile (<x)
