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

sumDigs :: Int -> Int
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



myLast :: [a] -> a
myLast [] = error "no last element -- no elements at all!"
myLast [x] = x
myLast (_:rst) = myLast rst

myButLast :: [a] -> a
myButLast [] = error "empty list -- need two elements"
myButLast [_] = error "only one element -- need two"
myButLast [x, _] = x
myButLast (_:rst) = myButLast rst

elementAt :: (Integral n) => [a] -> n -> a
elementAt _ n | n < 1 = error "index must be positive"
elementAt [] n = error "list too short"
elementAt (fst:_) 1 = fst
elementAt (_:rst) n = elementAt rst (n-1)

myLength :: (Integral n) => [x] -> n
myLength [] = 0
myLength (_:rst) = 1 + myLength rst

myReverse :: [x] -> [x]
myReverse x = reverseOnto x [] where
    reverseOnto [] reversed = reversed
    reverseOnto (mover:rest) reversed = reverseOnto rest (mover:reversed)

isPalindrome :: (Eq x) => [x] -> Bool
isPalindrome x = x == myReverse x

compress :: (Eq x) => [x] -> [x]
compress [] = []
compress [x] = [x]
compress (fst:nxt:rst)
    | fst == nxt = compress (fst:rst)
    | otherwise = (fst:compress(nxt:rst))

pack :: (Eq x) => [x] -> [[x]]
pack l = packHelp l [] where
    packHelp [] l = [l]
    packHelp (next:rst) [] = packHelp rst [next]
    packHelp (next:rst) (last:more)
        | next == last = packHelp rst (last:last:more)
        | otherwise = ((last:more):packHelp rst [next])


encode :: (Eq x, Integral y) => [x] -> [(y, x)]
encode = simplify . pack where
    simplify :: (Eq x, Integral y) => [[x]] -> [(y, x)]
    simplify [] = []
    simplify ([]:rst) = simplify rst
    simplify ((id:others):rst) = (1 + myLength others, id):simplify rst 

decode :: (Integral y) => [(y, x)] -> [x]
decode [] = []
decode ((0, _):rst) = decode rst
decode ((n, elem):rst) = elem:(decode $ (n-1,elem):rst)

myId :: (Eq x) => [x] -> [x]
myId = decode . encode

encodeDirect :: (Eq x, Integral y) => [x] -> [(y, x)]
encodeDirect [] = []
encodeDirect (first:rst) = encodeDirectHelper (1, first) rst where 
    encodeDirectHelper (count, current) [] = [(count, current)]
    encodeDirectHelper (count, current) (first:rst)
        | current == first = 
            encodeDirectHelper (count + 1, current) rst
        | otherwise = 
            (count, current):(encodeDirectHelper (1, first) rst)

myOtherId :: (Eq x) => [x] -> [x]
myOtherId = decode . encodeDirect

repeatParts :: (Integral n) => n -> [x] -> [x]
repeatParts _ [] = []
repeatParts n (first:rst) = repeatPartsHelper n first n rst where
    repeatPartsHelper 0 _ _ [] = []
    repeatPartsHelper 0 _ n (first:rst) = repeatPartsHelper n first n rst
    repeatPartsHelper left repeater n l =
        repeater:(repeatPartsHelper (left - 1) repeater n l)

dropEvery :: (Integral n) => n -> [x] -> [x]
dropEvery n l = dropEveryHelper (n - 1) n l where
    dropEveryHelper _ _ [] = []
    dropEveryHelper 0 n (fst:rst) = dropEveryHelper (n-1) n rst
    dropEveryHelper lft n (fst:rst) =
        fst:(dropEveryHelper (lft - 1) n rst)

splitInd :: (Integral n) => n -> [x] -> ([x], [x])
splitInd 0 l = ([], l)
splitInd _ [] = error "list too short!"
splitInd n (fst:rst) = (fst:l1, l2)
    where (l1, l2) = splitInd (n-1) rst

slice :: (Integral n) => n -> n -> [x] -> [x]
slice _ _ [] = []
slice a b (fst:rst)
    | a == b = []
    | a == 0 = fst:(slice a (b-1) rst)
    | otherwise = slice (a-1) (b-1) rst

rotate :: (Integral n) => n -> [x] -> [x]
rotate n l = l2 ++ l1 where
    (l1, l2) = splitInd n l

removeAt :: (Integral n) => n -> [x] -> [x]
removeAt _ [] = error "list too short!"
removeAt 0 (fst:rst) = rst
removeAt n (fst:rst) = fst:(removeAt (n-1) rst)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (fst:rst) = myFoldl f (f init fst) rst

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f init [] = init
myFoldr f init (fst:rst) = f fst (myFoldr f init rst)

insertAt :: (Integral n) => x -> [x] -> n -> [x]
insertAt elem l 0 = elem:l
insertAt _ [] _ = error "list too short!"
insertAt elem (fst:rst) n = fst:(insertAt elem rst (n-1))

range :: (Integral n) => n -> n -> [n]
range a b
    | a == b = []
    | otherwise = a:(range (a+1) b)

combinations :: (Integral n) => n -> [x] -> [[x]]
combinations 0 _ = [[]]
combinations n [] = []
combinations n (fst:rst) = withFst ++ woFst where
    withFst = (fst:) <$> combinations (n-1) rst
    woFst = combinations n rst

group :: (Integral n) => [n] -> [x] -> [[[x]]]
group [] _ = [[]]
group _ [] = [[]]
group (fst:rst) lst = foldr (++) [] $ map getallcombs firstcombs where
    firstcombs = combwrem fst lst
    getallcombs (comb, rem) = (comb:) <$> group rst rem

combwrem :: (Integral n) => n -> [x] -> [([x], [x])]
combwrem 0 l = [([], l)]
combwrem n [] = []
combwrem n (fst:rst) = withFst ++ woFst where
    appa x (a, d) = ((x:a), d)
    appd x (a, d) = (a, (x:d))
    withFst = (appa fst) <$> combwrem (n-1) rst
    woFst = (appd fst) <$> combwrem n rst

mySort :: (x -> x -> Ordering) -> [x] -> [x]
mySort _ [] = []
mySort f (fst:rst) = (mySort f less) ++ [fst] ++ (mySort f atleast) where
    less = filter (\x -> f x fst == LT) rst
    atleast = filter (\x -> not (f x fst == LT)) rst

sort :: (Ord x) => [x] -> [x]
sort = mySort compare

