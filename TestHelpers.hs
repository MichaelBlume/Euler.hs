import Test.QuickCheck
import Helpers
import Primes
import Onelines
import Data.List(intercalate)

testFibs :: [Int]
testFibs = fibs

prop_fibs :: Int -> Property
prop_fibs n = (n >= 0) ==> (n < 2000) ==> helper n where
  helper n = (testFibs !! n) + (testFibs !! (n + 1)) == (testFibs !! (n + 2))


join :: a -> [[a]] -> [a]
join x = intercalate [x]


are_inverse f g = isFixed (f . g)

prop_split_join :: (Char, String) -> Bool
prop_split_join (x,l) = isFixed (join x . split x) l

prop_split_removes :: (Char, String) -> Bool
prop_split_removes (x,l) = all (not . elem x) $ split x l

prop_primes_mult :: Int -> Property
prop_primes_mult n = (n < 10000) ==> prop n where
  prop = are_inverse product primeFactorization

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [a] = True
sorted (a:b:l) = and [a <= b, sorted (b:l)]

prop_sorted_factors :: Int -> Property
prop_sorted_factors n = (n < 10000) ==> sorted $ primeFactorization n

prop_prime_factors n = (n > 0) ==> (n < 100000000) ==> prop n where
  prop = all isPrime . primeFactorization

decode :: (Integral y) => [(y, x)] -> [x]
decode [] = []
decode ((0, _):rst) = decode rst
decode ((n, elem):rst) = elem:(decode $ (n-1,elem):rst)


prop_encode_decode :: (Eq x) => [x] -> Bool
prop_encode_decode = are_inverse decode encodeDirect


doTest name test = do
  putStrLn name
  quickCheck test

checkModel f g x = (f x) == (g x)

slowPrime x 
  | x < 2 = False
  | otherwise = all (not . divs x) [2..x-1]

prop_prime_model :: Int -> Property
prop_prime_model x = (x <= 20000) ==> (x >= 0) ==> prop x where
  prop = checkModel isPrime slowPrime

main = do
  doTest "check fibs" prop_fibs
  doTest "check split and join are id" prop_split_join
  doTest "check split element is removed" prop_split_removes
  doTest "check prime factors multiply correctly" prop_primes_mult
  doTest "check prime factors are sorted" prop_sorted_factors
  doTest "check prime factors are prime" prop_prime_factors
  doTest "check encodeDirect preserves info" (prop_encode_decode :: String -> Bool)
  doTest "check prime-checker works" prop_prime_model

