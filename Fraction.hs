module Fraction (Frac, numerator, denominator, fraction, invert) where

-- Fractions are always stored simplified, with positive denominators
-- To guarantee this, we do not export the data constructor
data Frac = Frac {numerator :: Integer, denominator :: Integer} deriving Show

fraction n d = Frac (div n c) (div d c) where
  c = mygcd n d
  mygcd n d
    | d >= 0 = gcd n d
    | otherwise = (gcd n d) * (-1)


instance Eq Frac

instance Num Frac where
  (+) (Frac n1 d1) (Frac n2 d2) = fraction (n1*d2 + n2*d1) (d1*d2)
  (*) (Frac n1 d1) (Frac n2 d2) = fraction (n1*n2) (d1*d2)
  abs (Frac n d) = Frac (abs n) d
  signum (Frac n _) = Frac (signum n) 1
  negate (Frac n d) = Frac (negate n) d
  fromInteger i = Frac (fromInteger i) 1

instance Ord Frac where
  compare (Frac n1 d1) (Frac n2 d2) = compare (n1 * d2) (n2 * d1)

invert (Frac n d) = Frac d n
