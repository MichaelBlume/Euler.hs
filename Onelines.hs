module Onelines
( square
, divs
, under
, pair
) where

square x = x*x

divs :: (Integral a) => a -> a -> Bool
divs a b = (mod a b) == 0

under x = takeWhile (<x)

pair a b = (a,b)
