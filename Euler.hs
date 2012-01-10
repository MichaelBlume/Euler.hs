module Main (main) where

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Data.Char (ord)
import Data.List (foldl', sort)

import Primes (primes, isPrime)
import Helpers (pow, maximizeFunc, sumProDivs, primeFactorization, fibs,
                split, encodeDirect, sumDigs)
import IOHelpers (getAndProcess, getIntGrid, getLines)
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
  goodTrips = filter isPythagorean $ enumerateDecTripletsSumming 1000

  enumerateDecTripletsSumming :: Int -> [(Int, Int, Int)]
  enumerateDecTripletsSumming sum = tripletsBelow sum where
    tripletsBelow max 
      | max*3 < sum = []
      | otherwise = (tripletsStarting max) ++ (tripletsBelow $ max - 1)
    tripletsStarting a = map makeTrip $ reverse [bottom..top] where
      bottom = (div (a-1) 2)+1
      top = min (a-1) (sum-a)
      makeTrip b = (a, b, sum-a-b)

  isPythagorean (a,b,c) = (a*a) == (b*b) + (c*c)

  threeProd (a, b, c) = a * b * c

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

dispatch 16 = print $ sumDigs $ pow 2 1000

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
      mydata = [(pow 10 12, "trillion"), (pow 10 9, "billion"),
                (pow 10 6, "million"), (1000, "thousand"), (100, "hundred")]

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

dispatch 18 = getAndProcess getIntGrid maxPath where
  nextSums :: [Int] -> [Int] -> [Int]
  nextSums prevSums newRow = zipWith max leftSums rightSums where
    leftSums = zipWith (+) (0:prevSums) newRow
    rightSums = zipWith (+) (prevSums ++ [0]) newRow

  lastRowSums :: [[Int]] -> [Int]
  lastRowSums = foldl' nextSums []

  maxPath :: [[Int]] -> Int
  maxPath = maximum . lastRowSums

dispatch 20 = print $ sumDigs $ facto 100 where
  facto 0 = 1
  facto 1 = 1
  facto n = n * (facto (n-1))

dispatch 21 = print $ sum $ filter amicable [1..9999] where

  amicable n = (n /= m) && (n == (sumProDivs m)) where
    m = sumProDivs n

dispatch 22 = getAndProcess getContents totalFromText where

  totalFromText = sum . map scoreNameTuple . enumerate . sort . namesFromText

  namesFromText :: String -> [String]
  namesFromText = everyOther . tail . split '"'

  everyOther :: [a] -> [a]
  everyOther [] = []
  everyOther (a:[]) = [a]
  everyOther (a:b:rst) = a:(everyOther rst)

  enumerate :: [a] -> [(Int, a)]
  enumerate = helper 0 where
    helper _ [] = []
    helper n (f:r) = (n,f):(helper (n+1) r)

  scoreName :: String -> Int
  scoreName = sum . map scoreChar

  ordA = ord 'A'
  scoreChar c = 1 + (ord c) - ordA

  scoreNameTuple (a, name) = (a + 1) * (scoreName name)


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
  bigNum = pow 10 999
  smallFibs = under bigNum fibs

dispatch 26 = print $ maximizeFunc firstDivisor [1..1000] where
  firstDivisor n = head $ filter (`divs` n) bigDivs
  bigDivs = map (\n -> n * 1024 * 3125) nines
  nines = 1:(map helper [1..])
  helper n = (pow 10 n) - 1

dispatch 27 = print $ (fst result) * (snd result) where
  result = maximizeFunc (lengthPrimes . quadratics) enumerateQuads
  lengthPrimes = length . takeWhile isPrime
  quadratics (a,b) = map (\n -> n*n + a*n + b) [0..]
  enumerateQuads = pair <$> [-999..999] <*> [-999..999]

dispatch 29 = print $ numDistincts $ pow <$> [2..100] <*> [2..100] where
    numDistincts :: (Eq a, Integral b) => [a] -> b
    numDistincts [] = 0
    numDistincts (n:ns) = 1 + (numDistincts rem) where
      rem = filter (/=n) ns

dispatch 30 = print $ sum $ myFilter $ [2..500000] where
  myFilter :: [Int] -> [Int]
  myFilter = filter $ isFixed $ sumNPowDigs 5

  isFixed :: (Eq a) => (a -> a) -> a -> Bool
  isFixed f a = (f a) == a

  sumNPowDigs n = sum . map (`pow` n) . map (\n -> read [n]) . show

dispatch x = putStrLn "Mike hasn't done that one yet."

main = do
  args <- getArgs
  dispatch $ read $ args !! 0
