module Main (main) where

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Data.List (sort, inits, tails, nub, delete, maximumBy)
import Data.Function (on)

import Primes (primes, isPrime)
import Helpers (maximizeFunc, sumProDivs, primeFactorization, fibs, maxPath
               ,isFixed, split, encodeDirect, sumDigs, digs, factorial
               ,pythagsSumming, wordsFromText, scoreChar)
import IOHelpers (getAndProcess, getIntGrid, getLines, takeIntGrid)
import Onelines (divs, under, pair, square)

dispatch :: Int -> IO ()
dispatch 1 = print $ sum $ filter divThreeOrFive $ [0..999] where
  divThreeOrFive :: Int -> Bool
  divThreeOrFive x = (mod x 5) * (mod x 3) == 0

dispatch 2 = print $ sum $ filter even $ under 4000000 fibs

dispatch 3 = print $ largestPrimeFactor 600851475143 where
  largestPrimeFactor :: Int -> Int
  largestPrimeFactor = head . reverse . primeFactorization

dispatch 4 = print $ head $ filter isThreeDigProduct enumerateSixPalindromes where
  palindromizeNum :: Int -> Int
  palindromizeNum x = read $ nums ++ (reverse nums) where
    nums = show x

  enumerateSixPalindromes :: [Int]
  enumerateSixPalindromes = map palindromizeNum $ reverse [100..999]

  isThreeDigProduct :: Int -> Bool
  isThreeDigProduct p = helper 999 where
    helper x
      | x * x < p = False
      | and [divs p x, (div p x) > 99] = True
      | otherwise = helper (x-1)

dispatch 5 = print $ foldr lcm 1 [1..20]

dispatch 6 = print $ (square $ sum [1..100]) - (sum $ map square [1..100])

--TIME: 39s
dispatch 7 = print result where
  result :: Int
  result = primes !! 10000

dispatch 8 = getAndProcess getBigStr $ biggestProductIn 5 where
  getBigStr = do
    putStrLn "Paste in the big digit-string"
    lines <- getLines
    return $ concat lines

  biggestProductIn :: Int -> String -> Int
  biggestProductIn n = maximum . map product . subLists n . map makeNum

  subLists :: Int -> [a] -> [[a]]
  subLists n l
    | n > length l = []
    | otherwise = (take n l):(subLists n $ tail l)

  makeNum :: Char -> Int
  makeNum c = read [c]

dispatch 9 = print $ threeProd $ head $ goodTrips where
  goodTrips = pythagsSumming 1000


  threeProd (a, b, c) = a * b * c

--TIME: >2m
dispatch 10 = print $ sum $ smallPrimes where
  smallPrimes :: [Int]
  smallPrimes = under 2000000 primes

dispatch 11 = getAndProcess getIntGrid $ getBiggestProduct 4 where

  getBiggestProduct length grid = maximum $ map product $ enumerateLines length grid

  enumerateLines length grid = concat sublists where
    sublists = map (linesForDir grid length) enumerateDirs

  enumerateDirs = [(1,0), (0,1), (1,1), (-1,1)]

  linesForDir :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
  linesForDir _ _ (0,0) = []
  linesForDir grid length dir = map (getVec grid length dir) $ goodCoords grid length dir

  getVec :: [[Int]] -> Int -> (Int, Int) -> (Int, Int) -> [Int]
  getVec _ 0 _ _ = []
  getVec grid length d@(dx, dy) (sx, sy) = (currNum:rest) where
    currNum = grid !! sx !! sy
    rest = getVec grid (length-1) d (sx+dx, sy+dy)

  goodCoords :: [[Int]] -> Int -> (Int, Int) -> [(Int, Int)]
  goodCoords grid mag (dx,dy) = zip <$> xr <*> yr where
    xr = goodRange width mag dx
    yr = goodRange height mag dy
    width = length grid
    height = length $ grid !! 0

    zip :: a -> b -> (a,b)
    zip a b = (a,b)

  goodRange :: Int -> Int -> Int -> [Int]
  goodRange full length 0 = [0..full-1]
  goodRange full length (-1) = [length-1..full-1]
  goodRange full length 1 = [0..full-length]


dispatch 12 = print $ bigTrigWithNDivs 501 where

  bigTrigWithNDivs :: Int -> Int
  bigTrigWithNDivs n = head $ filter (hasNDivs n) triangles

  hasNDivs :: Int -> Int -> Bool
  hasNDivs n bignum = n <= (countDivs bignum)

  countDivs :: Int -> Int
  countDivs = product . map (+1) . map fst . encodeDirect . primeFactorization

  triangles :: [Int]
  triangles = triHelper 1 2 where
    triHelper tri nat = (tri:rest) where
      rest = triHelper (tri+nat) (nat+1)

dispatch 13 = do
  putStrLn "Paste in all the numbers, then a blank"
  lines <- getLines
  putStrLn $ (take 10) . show . sum . (map read) $ lines

--TIME: 15s
dispatch 14 = print $ maximizeFunc collatzLength [1..999999] where
  collatzLength :: Int -> Int
  collatzLength 1 = 1
  collatzLength a = 1 + (collatzLength next) where
    next = if (divs a 2)
      then div a 2
      else 3*a + 1

  collatzDec n = (collatzLength n, n)


dispatch 15 = print $ countPaths 20 20 where
  countPaths a b = pathsData !! (max a b) !! (min a b) where
    pathsData = map helper [0..]
    helper a = map (countPathsMan a) [0..]
    countPathsMan 0 _ = 1
    countPathsMan _ 0 = 1
    countPathsMan a b = (countPaths (a-1) b) + (countPaths a (b-1))

dispatch 16 = print $ sumDigs $ 2 ^ 1000

dispatch 17 = print $ sum $ map lettersInNumber [1..1000] where

  lettersInNumber :: Int -> Int
  lettersInNumber = length . filter (not . (`elem` " -")) . numberToText

  numberToText :: Int -> String
  numberToText = ntt where
    ntt 0 = "zero"
    ntt 1 = "one"
    ntt 2 = "two"
    ntt 3 = "three"
    ntt 4 = "four"
    ntt 5 = "five"
    ntt 6 = "six"
    ntt 7 = "seven"
    ntt 8 = "eight"
    ntt 9 = "nine"
    ntt 10 = "ten"
    ntt 11 = "eleven"
    ntt 12 = "twelve"
    ntt 13 = "thirteen"
    ntt 15 = "fifteen"
    ntt 18 = "eighteen"
    ntt x | x < 20 = ntt (x - 10) ++ "teen"
    ntt 20 = "twenty"
    ntt 30 = "thirty"
    ntt 40 = "forty"
    ntt 50 = "fifty"
    ntt 60 = "sixty"
    ntt 70 = "seventy"
    ntt 80 = "eighty"
    ntt 90 = "ninety"
    ntt x | x < 100 = major ++ minor remn where
      major :: String
      major = ntt (x - remn)

      minor :: Int -> String
      minor 0 = ""
      minor x = '-':ntt x

      remn :: Int
      remn = x `rem` 10
    ntt x = helper x mydata where
      mydata = [(10 ^ 12, "trillion"), (10 ^ 9, "billion"),
                (10 ^ 6, "million"), (1000, "thousand"), (100, "hundred")]

      helper x [] = ntt x
      helper x ((num, name):ps)
        | x < num = helper x ps
        | otherwise = major ++ minor remn where
            major = (ntt $ div x num) ++ ' ':name
            minor 0 = ""
            minor x
              | num == 100 = " and " ++ ntt x
              | otherwise = ' ':ntt x
            remn = x `rem` num

dispatch 18 = getAndProcess getIntGrid maxPath

dispatch 20 = print $ sumDigs $ factorial 100 where

dispatch 21 = print $ sum $ filter amicable [1..9999] where

  amicable n = (n /= m) && (n == (sumProDivs m)) where
    m = sumProDivs n

dispatch 22 = getAndProcess getContents totalFromText where

  totalFromText = sum . map scoreNameTuple . enumerate . sort . wordsFromText

  enumerate :: [a] -> [(Int, a)]
  enumerate = helper 0 where
    helper _ [] = []
    helper n (f:r) = (n,f):(helper (n+1) r)

  scoreName :: String -> Int
  scoreName = sum . map scoreChar


  scoreNameTuple (a, name) = (a + 1) * (scoreName name)


--TIME: 13s
dispatch 23 = print $ sum $ filter (not . abundantSum) [1..28123] where
  isAbundant n = n < (sumProDivs n)

  abundants = filter isAbundant [1..]

  abundantSum n = any isAbundant $ remainders where
    remainders = map (n-) $ under n $ atLeast ((div n 2) - 1) abundants

  atLeast n = dropWhile (<n)

dispatch 24 = putStrLn $ (perms ['0'..'9']) !! 999999 where
  perms :: (Eq a) => [a] -> [[a]]
  perms [] = [[]]
  perms l= concat $ map (permsNFirst l) l where
    permsNFirst l n = map (n:) $ perms $ filter (/=n) l

dispatch 25 = print $ 1 + (length smallFibs) where
  bigNum = 10 ^ 999
  smallFibs = under bigNum fibs

dispatch 26 = print $ maximizeFunc firstDivisor [1..1000] where
  firstDivisor n = head $ filter (`divs` n) bigDivs
  bigDivs = map (\n -> n * 1024 * 3125) nines
  nines = 1:(map helper [1..])
  helper n = (10 ^ n) - 1

--TIME: 1m13s
dispatch 27 = print $ (fst result) * (snd result) where
  result = maximizeFunc (lengthPrimes . quadratics) enumerateQuads
  lengthPrimes = length . takeWhile isPrime
  quadratics (a,b) = map (\n -> n*n + a*n + b) [0..]
  enumerateQuads = pair <$> [-999..999] <*> [-999..999]

dispatch 29 = print $ length $ nub $ (^) <$> [2..100] <*> [2..100]

--TIME: 14s
dispatch 30 = print $ sum $ myFilter $ [2..500000] where
  myFilter :: [Int] -> [Int]
  myFilter = filter $ isFixed $ sumNPowDigs 5

  sumNPowDigs n = sum . map (^ n) . map (\n -> read [n]) . show

dispatch 31 = print $ waysToMakeEng 200 where

  waysToMakeEng = waysToMake [200, 100, 50, 20, 10, 5, 2, 1]

  waysToMake :: [Int] -> Int -> Int
  waysToMake _ x | x < 0 = 0
  waysToMake _ 0 = 1
  waysToMake [] _ = 0
  waysToMake [1] _ = 1
  waysToMake cs@(c:rcs) x = withFirst + withoutFirst where
    withFirst = waysToMake cs (x-c)
    withoutFirst = waysToMake rcs x

--TIME: 1m30
dispatch 34 = print $ sum $ filter isCurious $ [3..2540161] where
  isCurious = isFixed $ sum . map factorial . digs

--TIME: 17s
dispatch 35 = print $ length $ filter circular $ under 1000000 primes where
  circular = all isPrime . tail . rotationsN
  rotationsN = map read . rotations . show
  rotations xs = take l $ map (take l) $ iterate tail $ cycle xs where
    l = length xs

--TIME: 2.9s
dispatch 36 = print $ sum $ filter isTwoTenPal [0..999999] where
  extractWith :: (a -> Maybe (a, b)) -> a -> [b]
  extractWith f x = case f x of
    Nothing -> []
    Just (x', y) -> y:(extractWith f x')

  peelDigit :: Int -> Maybe (Int, Int)
  peelDigit 0 = Nothing
  peelDigit x
    | divs x 2 = Just (div x 2, 0)
    | otherwise = Just (div x 2, 1)

  binarize = extractWith peelDigit

  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome = isFixed reverse

  isTwoTenPal x = and [isPalindrome $ binarize x, isPalindrome $ show x]

--TIME: 10s
dispatch 37 = print $ sum $ take 11 $ filter truncateable bigPrimes where

  bigPrimes = dropWhile (<10) primes

  truncateable :: Int -> Bool
  truncateable = all isPrime . frags

  frags = map read . applyBothWith assemble inits tails . show

  dropEnd = delete []

  assemble = on (++) dropEnd

  applyBothWith :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
  applyBothWith c f g x = c (f x) (g x)

dispatch 38 = print $ maximum panDigitals where
  products x = map (*x) [1..]
  prodBlobs = map (concat . map show) . inits . products
  firstCand = head . dropWhile ((<9) . length) . prodBlobs
  candidates = map firstCand [1..9999]

  panDigital = (=="123456789") . sort . nub

  panDigitals :: [Int]
  panDigitals = map read $ filter panDigital $ filter ((==9) . length) $ candidates

--TIME: 1.6s
dispatch 39 = print $ maxOn numSols [5..1000] where
  numSols = length . pythagsSumming
  decorate f = map (\n -> (f n, n))
  maxOn key = snd . maximumBy (compare `on` fst) . decorate key

dispatch 40 = print $ product $ map digitAt $ map (10^) $ [0..6] where
  digits = concat $ map show [1..]

  digitAt :: Int -> Int
  digitAt n = read [digits !! (n - 1)]

dispatch 41 = print bigPandigitalPrime where
  panDigital n = all (`elem` ds) [1..l] where
    ds = digs n
    l = length ds

  bigPandigitalPrime = head $ filter panDigital $ filter isPrime [7654321,7654320..1]

dispatch 42 = getAndProcess getContents countTriWords where
  countTriWords :: String -> Int
  countTriWords = length . filter triWord . wordsFromText

  triWord :: String -> Bool
  triWord = triangular . score

  score :: String -> Int
  score = sum . map scoreChar

  triangular :: Int -> Bool
  triangular = inSortedList triangles

  inSortedList [] _ = False
  inSortedList (n:ns) x
    | x < n = False
    | x == n = True
    | otherwise = inSortedList ns x

  triangles = map (\n -> div (n * (n+1)) 2) [1..]

dispatch 47 = print $ head $ nSeqWith 4 (hasNPrimes 4) [1..] where
  nSeqWith length pred l = helper 0 [] l where
    helper count cache (x:xs)
      | count == length = reverse cache
      | pred x = helper (count + 1) (x:cache) xs
      | otherwise = helper 0 [] xs

  hasNPrimes n = (>=n) . length . nub . primeFactorization

dispatch 67 = getAndProcess takeIntGrid maxPath

dispatch x = putStrLn "Mike hasn't done that one yet."

main = do
  args <- getArgs
  dispatch $ read $ args !! 0
