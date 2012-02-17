module Memoize (memoizeF) where

import Onelines (divs)

data InfiniteTree a = ITree a (InfiniteTree a) (InfiniteTree a)

iNums :: (Integral n) => InfiniteTree n
iNums = construct 0 1 where
  construct start step = ITree start left right where
    left = construct (start + step) (step * 2)
    right = construct (start + step * 2) (step * 2)

instance Functor InfiniteTree where
  fmap f (ITree a left right) = ITree (f a) (fmap f left) (fmap f right)

seek :: (Integral i) => InfiniteTree a -> i -> a
seek (ITree a _ _) 0 = a
seek (ITree _ l r) n
  | divs n 2 = seek r $ div n 2 - 1
  | otherwise = seek l $ div n 2

memoizeF :: (Integral n) => (n -> a) -> n -> a
memoizeF f = seek myData where
  myData = fmap f iNums
