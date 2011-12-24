import Control.Applicative
import System
import Data.Char

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
group (fst:rst) lst = foldl (++) [] $ map getallcombs firstcombs where
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

divs :: (Integral a) => a -> a -> Bool
divs a b = (mod a b) == 0

isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime x = not $ any (divs x) $ primesBelowSqrt x

primesBelowSqrt :: (Integral a) => a -> [a]
primesBelowSqrt square = pbHelp primeMap where
    pbHelp ((num, prime):rst)
        | num * num > square = []
        | not prime = pbHelp rst
        | otherwise = num:(pbHelp rst)

primeMap :: (Integral a) => [(a, Bool)]
primeMap = map (\x -> (x, isPrime x)) [2..]

primes :: (Integral a) => [a]
primes = map first $ filter second $ primeMap

main = print $ primes !! 100000

divThreeOrFive :: Int -> Bool
divThreeOrFive x = (mod x 5) * (mod x 3) == 0

probOneResult = sum $ filter divThreeOrFive $ range 0 1000

fibs :: [Int]
fibs = 1:1: (zipWith (+) (tail fibs) fibs)

under :: Int -> [Int] -> [Int]
under x = takeWhile (<x)

probTwoResult = sum $ filter even $ under 4000000 fibs

primeFactorization :: Int -> [Int]
primeFactorization = helper primes where
  helper _ 1 = []
  helper (fp:rp) n
    | divs n fp = fp:(helper (fp:rp) (div n fp))
    | otherwise = helper rp n

largestPrimeFactor :: Int -> Int
largestPrimeFactor = head . reverse . primeFactorization

probThreeResult = largestPrimeFactor 600851475143

palindromizeNum :: Int -> Int
palindromizeNum x = read $ nums ++ (reverse nums) where
  nums = show x

enumerateSixPalindromes :: [Int]
enumerateSixPalindromes = map palindromizeNum $ reverse [100..999]

isThreeDigProduct :: Int -> Bool
isThreeDigProduct p = isThreeDigProductOne 999 where
  isThreeDigProductOne x
    | x * x < p = False
    | and [divs p x, (div p x) > 99] = True
    | otherwise = isThreeDigProductOne (x-1)

probFourResult = head $ filter isThreeDigProduct enumerateSixPalindromes

probFiveResult = foldl lcm 1 [1..20] 

square x = x*x

probSixResult = (square $ sum [1..100]) - (sum $ map square [1..100])

probSevenResult = primes !! 10000

getLines :: IO [[Char]]
getLines = do
  line <- getLine
  if null line
      then return []
      else do
          rest <- getLines
          return (line:rest)

getBigStr = do
  lines <- getLines
  return $ foldl (++) "" lines

makeNum :: Char -> Int
makeNum c = read [c]

subLists :: Int -> [a] -> [[a]]
subLists n l
  | n > length l = []
  | otherwise = (take n l):(subLists n $ tail l)

biggestProductIn :: String -> Int
biggestProductIn = maximum . map product . subLists 5 . map makeNum

doProbEight = do
  putStrLn "Paste the large number, followed by a blank line."
  str <- getBigStr
  putStrLn $ show $ biggestProductIn str

enumerateDecTripletsSumming :: Int -> [(Int, Int, Int)]
enumerateDecTripletsSumming sum = tripletsBelow sum where
  tripletsBelow max 
    | max*3 < sum = []
    | otherwise = (tripletsStarting max) ++ (tripletsBelow $ max - 1)
  tripletsStarting a = map makeTrip $ reverse [(div (a-1) 2)+1..top] where
    top = min (a-1) (sum-a)
    makeTrip b = (a, b, sum-a-b)

isPythagorean (a,b,c) = (a*a) == (b*b) + (c*c)

threeProd (a, b, c) = a * b * c

probNineResult = threeProd $ head $ filter isPythagorean $ enumerateDecTripletsSumming 1000

probTenResult = sum $ under 2000000 primes

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split s (h:t)
  | s == h = []:(split s t)
  | otherwise = (h:first):rest where
      (first:rest) = split s t

parseIntLine :: String -> [Int]
parseIntLine = map read . split ' '

getIntGrid = do
  putStrLn "Paste the big grid, then blank line"
  lines <- getLines
  return $ map parseIntLine lines

doProbEleven = getAndProcess getIntGrid $ getBiggestProduct 4

getBiggestProduct length grid = maximum $ map product $ enumerateLines length grid

enumerateLines length grid = foldl (++) [] $ map (linesForDir grid length) enumerateDirs

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
zipCombs l1 l2 = foldl (++) [] $ map helper1 l2 where
  helper1 n2 = map helper2 l1 where
    helper2 n1 = (n1, n2)

bigTrigWithNDivs :: Int -> Int
bigTrigWithNDivs n = head $ filter (hasNDivs n) triangles

hasNDivs :: Int -> Int -> Bool
hasNDivs n bignum = n <= (countDivs bignum)

countDivs :: Int -> Int
countDivs = product . map (+1) . map first . encodeDirect . primeFactorization

first (a,b) = a
second (a,b) = b

allProds a b = (*) <$> a <*> b

nonDetProduct = foldl allProds [1]

getPows (n,b) = map (pow b) [0..n]

allDivs = nonDetProduct . (map getPows) . encodeDirect . primeFactorization

triangles :: [Int]
triangles = triHelper 1 2 where
  triHelper tri nat = (tri:rest) where
    rest = triHelper (tri+nat) (nat+1)

probTwelveResult :: Int
probTwelveResult = bigTrigWithNDivs 501

doProbThirteen = getAndProcess getLines $ (take 10) . show . sum . (map read)

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength a = 1 + (collatzLength next) where
  next = if (divs a 2)
    then div a 2
    else 3*a + 1

collatzDec n = (collatzLength n, n)

probFourteenResult = second $ maximum $ map collatzDec [1..999999]

probFifteenResult = countPaths 20 20

countPaths a b = pathsData !! (max a b) !! (min a b) where
  pathsData = map helper [0..]
  helper a = map (countPathsMan a) [0..]
  countPathsMan 0 _ = 1
  countPathsMan _ 0 = 1
  countPathsMan a b = (countPaths (a-1) b) + (countPaths a (b-1))

sumDigs = sum . map (\n -> read [n]) . show

pow _ 0 = 1
pow a b
  | divs b 2 = pow (a*a) (div b 2)
  | otherwise = a * (pow a (b-1))

probSixteenResult :: Int
probSixteenResult = sumDigs $ pow 2 1000

nextSums :: [Int] -> [Int] -> [Int]
nextSums prevSums newRow = zipWith max leftSums rightSums where
  leftSums = zipWith (+) (0:prevSums) newRow
  rightSums = zipWith (+) (prevSums ++ [0]) newRow

lastRowSums :: [[Int]] -> [Int]
lastRowSums = foldl nextSums []

maxPath :: [[Int]] -> Int
maxPath = maximum . lastRowSums

getAndProcess :: (Show b) => IO a -> (a -> b) -> IO ()
getAndProcess getter processor = do
  intermediate <- getter
  print $ processor intermediate

doProbEighteen :: IO ()
doProbEighteen = getAndProcess getIntGrid maxPath

facto 0 = 1
facto 1 = 1
facto n = n * (facto (n-1))

probNineteenResult = sumDigs $ facto 100

sumProDivs = (myData !!) where
  myData = map helper [0..]
  helper 0 = 0
  helper n = (sum $ allDivs n) - n

amicable n = (n /= m) && (n == (sumProDivs m)) where
  m = sumProDivs n

prob21Result = sum $ filter amicable [1..9999]

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

doProb22 = getAndProcess getContents totalFromText

isAbundant n = n < (sumProDivs n)

abundants = filter isAbundant [1..]

abundantSum n = any isAbundant $ remainders where
  remainders = map (n-) $ under n $ atLeast ((div n 2) - 1) abundants

prob23Result = sum $ filter (not . abundantSum) [1..28123]

atLeast n = dropWhile (<n)








sieve :: [Int] -> [Int]
sieve ns = n : sieve ns'
	where
		n = head ns
		ns' = filter ((/= 0) . flip rem n) ns

primes2 :: [Int]
primes2 = sieve [2..]

isPrimeNaive :: Int -> Bool
isPrimeNaive 2 = True
isPrimeNaive n = not $ any (divs n) [2..(n-1)]

primes3 = filter isPrimeNaive [2..]

rewrap :: (Monad m) => [m a] -> m [a]
rewrap [] = return []
rewrap (n:ns) = do
  o <- n
  os <- rewrap ns
  return (o:os)

prompt s = do
  putStrLn $ show s
  result <- getLine
  return result

getPrompts l = rewrap $ map prompt l

getFivePrompts = do
  result <- getPrompts [1..5]
  putStrLn $ show result

getOnePrime = do
  primeNum <- getLine
  irrelevantThing <- getLine
  result <- return $ primes !! (read primeNum)
  putStrLn $ show $ result
