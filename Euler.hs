import System.Environment
import Data.Char
import Data.List

import Primes
import Helpers
import IOHelpers
import Onelines

dispatch :: Int -> IO ()
dispatch 1 = print $ sum $ filter divThreeOrFive $ [0..999] where
  divThreeOrFive :: Int -> Bool
  divThreeOrFive x = (mod x 5) * (mod x 3) == 0

dispatch 2 = print result where
  result :: Int
  result = sum $ filter even $ under 4000000 fibs

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
    lines <- getLines
    return $ foldr (++) "" lines

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

  enumerateLines length grid = foldr (++) [] sublists where
    sublists = map (linesForDir grid length) enumerateDirs

  enumerateDirs = [(1,0), (0,1), (1,1), (-1,1)]

  linesForDir :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
  linesForDir _ _ (0,0) = []
  linesForDir grid length dir = map (getVec grid length dir) $ goodCoords grid length dir

  getVec :: [[Int]] -> Int -> (Int, Int) -> (Int, Int) -> [Int]
  getVec _ 0 _ _ = []
  getVec grid length (dx, dy) (sx, sy) = (currNum:rest) where
    currNum = grid !! sx !! sy
    rest = getVec grid (length-1) (dx, dy) (sx+dx, sy+dy)

  goodCoords :: [[Int]] -> Int -> (Int, Int) -> [(Int, Int)]
  goodCoords grid mag (dx,dy) = zipCombs xr yr where
    xr = goodRange width mag dx
    yr = goodRange height mag dy
    width = length grid
    height = length $ grid !! 0

  goodRange :: Int -> Int -> Int -> [Int]
  goodRange full length 0 = [0..full-1]
  goodRange full length (-1) = [length-1..full-1]
  goodRange full length 1 = [0..full-length]

  zipCombs :: [a] -> [b] -> [(a,b)]
  zipCombs l1 l2 = foldr (++) [] $ map helper1 l2 where
    helper1 n2 = map helper2 l1 where
      helper2 n1 = (n1, n2)

dispatch 12 = print $ bigTrigWithNDivs 501 where

  bigTrigWithNDivs :: Int -> Int
  bigTrigWithNDivs n = head $ filter (hasNDivs n) triangles

  hasNDivs :: Int -> Int -> Bool
  hasNDivs n bignum = n <= (countDivs bignum)

  countDivs :: Int -> Int
  countDivs = product . map (+1) . map first . encodeDirect . primeFactorization

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
  everyOther :: [a] -> [a]
  everyOther [] = []
  everyOther (a:[]) = [a]
  everyOther (a:b:rst) = a:(everyOther rst)

  namesFromText :: String -> [String]
  namesFromText = everyOther . tail . split '"'

  enumerate :: [a] -> [(Int, a)]
  enumerate = helper 0 where
    helper _ [] = []
    helper n (f:r) = (n,f):(helper (n+1) r)

  scoreName :: String -> Int
  scoreName = sum . map scoreChar

  ordA = ord 'A'
  scoreChar c = 1 + (ord c) - ordA

  scoreNameTuple (a, name) = (a + 1) * (scoreName name)

  totalFromText = sum . map scoreNameTuple . enumerate . sort . namesFromText

dispatch 23 = print $ sum $ filter (not . abundantSum) [1..28123] where
  isAbundant n = n < (sumProDivs n)

  abundants = filter isAbundant [1..]

  abundantSum n = any isAbundant $ remainders where
    remainders = map (n-) $ under n $ atLeast ((div n 2) - 1) abundants

  atLeast n = dropWhile (<n)

dispatch 24 = putStrLn $ (perms "0123456789") !! 999999 where
  perms :: (Eq a) => [a] -> [[a]]
  perms [] = [[]]
  perms l= foldr (++) [] $ map (permsNFirst l) l where
    permsNFirst l n = map (n:) $ perms $ filter (/=n) l

dispatch 25 = print $ 1 + (length smallFibs) where
  bigNum = pow 10 999
  smallFibs = under bigNum fibs

dispatch 26 = print $ maximizeFunc firstDivisor [1..1000] where
  firstDivisor n = head $ filter ((flip divs) n) bigDivs
  bigDivs = map (\n -> n * 1024 * 3125) nines
  nines = 1:(map helper [1..])
  helper n = (pow 10 n) - 1

dispatch 27 = print $ (first result) * (second result) where
  lengthPrimes = length . takeWhile isPrime
  quadratics (a,b) = map (\n -> n*n + a*n + b) [0..]
  result = maximizeFunc (lengthPrimes . quadratics) enumerateQuads
  enumerateQuads = foldr (++) [] $ map helper [-999..999]
  helper a = map (\b -> (a,b)) [-999..999]

main = do
  args <- getArgs
  dispatch $ read $ args !! 0
