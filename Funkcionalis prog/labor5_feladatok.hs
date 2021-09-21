--Labor 5. feladat megoldasai
--I. 
import Data.Char
import Data.List
import Data.Ord

--1.
parosSzamNegyzete n
    | n == 0 = []
    | otherwise = [i^2 | i<-[1..n], i `mod` 2 == 0]
    
--2.
szamok n = [i | i<-[1..n], k<-[1..i]]

--3.
paros n = [i | i<-[2,4..n], k<-[1..(i`div`2)]] 
 
--4.
sorozat n = [i | i<-reverse([1..n])++[0]++[1..n]]

--5.
fel1_5 :: Int -> [Bool]
fel1_5 n = take n fugv
    where fugv = (True: (False: fugv))

--6.
fel1_6 n = take n fugv
    where fugv = (0: (1: (-1: fugv)))
    
--II.
--1.
osztok nr = [i | i<-[1..nr], nr `mod` i == 0]
    
felII_1 nr = length(osztok nr)

--2.
felII_2 nr = maximum (filter odd (osztok nr))

--2.
tukrozes ls = ls ++ (reverse ls)

--3.
convFromNr nr b
    | nr < b = [nr]
    | otherwise = (nr `mod` b): (convFromNr (nr `div` b) b)

szamjegyszam nr p = length(convFromNr nr p)

--4.
maxSzam nr p = maximum (convFromNr nr p)

--5.
felII_5 nr ls
    | ls == [] = []
    | otherwise = k: (felII_5 nr ve)
        where k:ve = drop (nr-1) ls

--6.???

--7.
fibonacci a b = fib a b 1 2
fib a b x y 
    | x >= b = []
    | x >= a = x : fib a b y (x+y)
    | otherwise = fib a b y (x+y)

--8.
ossz [] = 0
ossz (k:ve)
    | k > 0 = k + ossz ve
    | otherwise = ossz ve

db [] = 0
db (k:ve)
    | k > 0 = 1 + db ve
    | otherwise = db ve

atlag ls = fromIntegral (ossz ls) / fromIntegral (db ls)

--9.
osszeg [] = 0
osszeg ((a,b):ve) = b + osszeg ve

--10.
elemTalalat nr [] = []
elemTalalat nr (k:ve)
    | nr == k = k: elemTalalat nr ve
    | otherwise = elemTalalat nr ve

--III.
--1.
getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

fiboN n= takeWhile ((>)n) fibo
fibo = map fst (iterate f (0,1)) where f (x,y) = (y,x+y)

fibonacci2 = do
    putStr "szam = "
    sz1 <- getInt
    let fibb = fiboN sz1
    putStrLn (show fibb)

--2.
eszita :: Int -> [Int]
eszita n = takeWhile ((>) n) (2: szita [3, 5 ..])

szita :: [Int] -> [Int]
szita (k: ve) = (k: szita [x | x <- ve, mod x k /= 0])

primszamok = do
    putStr "szam = "
    sz1 <- getInt
    let prim = eszita sz1
    putStrLn (show prim)

--3.
conv :: Int -> Int -> [Int]
conv nr p = sConv nr p []
    where
    sConv nr p res
        | nr < p = (nr: res)
        | otherwise = sConv (div nr p) p (mod nr p: res)

feloszt ls valaszto
    | length ls == valaszto = ls
    | otherwise = take valaszto ls ++ " " ++ feloszt (drop valaszto ls) valaszto

feloszt2 = do
    putStr "szam = "
    sz1 <- getInt
    let oszt = conv sz1 2
    let seged = map intToDigit oszt
    let oszt2 = feloszt (reverse seged) 4
    putStrLn(show (reverse oszt2))

--4.
szimb :: Int -> Char
szimb c =
    case c of
        10 -> 'a'
        11 -> 'b'
        12 -> 'c'
        13 -> 'd'
        14 -> 'e'
        15 -> 'f'
        _ -> chr (c + 48)
        
conv16 :: Int -> [Char]
conv16 nr = map szimb (conv nr 16)

oszt16 = do 
    putStr "szam = "
    sz1 <- getInt
    let oszt1 = conv16 sz1
    let oszt2 = feloszt (reverse oszt1) 2
    putStrLn (show (reverse oszt2))

--5.
hatv1 x n 
    | n == 0 =1
    | n `mod` 2 == 0 = temp * temp
    | otherwise = x * temp * temp
        where 
            temp = hatv1 x (n `div` 2)

hatv = do
    putStr "n: "
    n <- getInt
    putStr "x: "
    x <- getInt
    let hatvvv = hatv1 n x
    putStrLn (show hatvvv)

--IV.
olvasSor :: IO[Integer]
olvasSor = do
    lista <- getLine
    let nL = map (read :: String -> Integer) (words lista)
    return nL

felIV_1 = do
    putStr "Szamok: "
    ls <- olvasSor
    let maxi = maximum ls
    putStrLn (show maxi)

--2.
maxPozII :: (Ord a) => [a] -> (a, [Int])
maxPozII [] = error "Ures lista"
maxPozII (k: ve) = smaxPoz k ve 1 [0]
    where
    smaxPoz :: (Ord a) => a -> [a] -> Int -> [Int] -> (a, [Int])
    smaxPoz m [] ind res = (m, res)
    smaxPoz m (k: ve) ind res
        | m == k = smaxPoz m ve (ind + 1) (ind: res)
        | m > k = smaxPoz m ve (ind + 1) res
        | m < k = smaxPoz k ve (ind + 1) [ind]

felIV_2 = do
    putStr "Szamok: "
    ls <- olvasSor
    let maxpoz = maxPozII ls
    putStrLn (show maxpoz)

--3.
ins x [] = [x]
ins x (k: ve)
    |x > k = (k: ins x ve)
    | otherwise = (x: (k: ve))

insertS [] = []
insertS (k: ve) = ins k (insertS ve)

felIV_3 = do
    putStr "Szamok: "
    ls <- olvasSor
    let rendezve = insertS ls
    putStrLn (show rendezve)