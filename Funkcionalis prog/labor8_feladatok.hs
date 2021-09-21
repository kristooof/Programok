--Labor 8 feladatok
import System.IO

--1.
lexi :: Int -> Int -> [[Char]]
lexi 0 n = [[]]
lexi m n = [(k: ve) | k <- take n ['a'..'z'], ve <- lexi (m-1) n]

--2??
getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)
 
kombinacio :: Int -> Int-> [[Char]]
kombinacio n m = skomb n m

skomb :: Int -> Int -> [[Char]]
skomb n 0 = [[]]
skomb n m = [(k: ve) | k <- take n ['a'..'z'], ve <- skomb n (m-1), feltK k ve]

feltK :: Char -> [Char] -> Bool
feltK k1 [] = True
feltK k1 (k: ve)
    |k >= k1 = False
    |otherwise = feltK k1 ve
    
lexi2 = do
    putStr "n = "
    n <- getInt
    putStr "m = "
    m <- getInt
    outf <- openFile "lexi.txt" WriteMode
    let lista = skomb n m
    --putStr (show lista)
    hPutStr outf (show lista)
--3
queen :: Int -> [[Int]]
queen n = squeen n n

squeen :: Int -> Int -> [[Int]]
squeen n 0 = [[]]
squeen n m = [(k: ve) | k <- [1..n], ve <- squeen n (m-1),
    feltQ 1 k ve ]

feltQ :: Int -> Int -> [Int] -> Bool
feltQ i k1 [] = True
feltQ i k1 (k: ve)
    | k == k1 = False
    | abs (k1 - k) == i = False
    | otherwise = feltQ (i+1) k1 ve

kdikKiralyno = do  
    putStr "n = "
    n <- getInt
    putStr "k = "
    k <- getInt
    let kiralynok = queen n
    let kdik = kiralynok !! k
    putStr (show kdik)

--4
spascal [k] = [k]
spascal (k1: (k2: ve)) = ((k1 + k2): spascal (k2: ve))

pascal :: Int -> [Int]
pascal 0 = spascal [1]
pascal n = (1: (spascal $ pascal (n-1)))

kdikElem n k = (pascal n) !! k

--5
mymerge :: [Integer] -> [Integer] -> [Integer]
mymerge [] [] = []
mymerge list [] = list
mymerge [] list = list
mymerge (k1: ve1) (k2: ve2)
    |k1 < k2 = (k1: mymerge ve1 (k2: ve2))
    |k1 == k2 = mymerge (k1: ve1) ve2
    |k1 > k2 = (k2: mymerge (k1: ve1) ve2)

ham :: [Integer]
ham = (1: lista)
    where
    hamL = ham
    lista1 = [2*h | h <- hamL]
    lista2 = [3*h | h <- hamL]
    lista = mymerge lista1 lista2

abkoztiHamingek a b = dropWhile (<a) (takeWhile (<b) ham)

--6
eszita :: Int -> [Int]
eszita n = takeWhile ((>)n) (2: szita [3, 5..])

szita :: [Int] -> [Int]
szita (k:ve) = (k: szita [x | x <- ve, mod x k /= 0])

primek = do --dropWhile ((>)n) (eszita m)
    outf <- openFile "primek.txt" WriteMode
    let prim = dropWhile ( > 1000) (eszita 1000)
    hPutStr outf (show prim)
    
--7
data Datum = Datum{
    nap :: Int,
    honap:: Int,
    ev :: Int
    } deriving (Show)

data Szemely = Szemely {
    vnev :: [Char],
    knev :: [Char],
    szdatum :: Datum
    } deriving (Show)

keresF1 :: Szemely -> (String, String)
keresF1 elem = (vnev elem++" "++knev elem, hetSzulnap (szdatum elem))

hetSzulnap:: Datum -> String
hetSzulnap elem
    = ["Vasarnap","Hetfo","Kedd","Szerda","Csutortok","Pentek","Szombat"] !! k
    where
    k = napsz (nap elem) (honap elem) (ev elem)
    
napsz :: Int -> Int -> Int -> Int
napsz n h e
    = ( (e-1) * 365
    + (e-1) `div` 4 - (e-1) `div` 100 + (e-1) `div` 400
    + sum (take (h-1) (honapok e))
    + n
    )`rem` 7
    
honapok :: Int -> [Int]
honapok y = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    where
    feb
        | szokoev y = 29
        | otherwise = 28

szokoev :: Int -> Bool
szokoev y
    | y `rem` 100 == 0 = (y `rem` 400==0)
    | otherwise = (y `rem` 4 == 0)

mainF1 :: (String, String)
mainF1 = keresF1 egySz

{-mainF1 :: (String, String)
mainF1 = do
    inf <- openFile "szemely.txt" ReadMode
    lista <- feldolgoz inf
    let res = keresF1 lista-}

feldolgoz :: Handle -> IO [Szemely]
feldolgoz inf = do
    eof <- hIsEOF inf
    if eof then return []
    else do
        sor <- hGetLine inf
        let lSor = words sor
        let vNev = lSor !! 0
        let kNev = lSor !! 1
        let nap = (read (lSor !! 2) :: Int)
        let honap = (read (lSor !! 3) :: Int)
        let ev = (read (lSor !! 4) :: Int)
        let elemD = Datum nap honap ev
        let elemL = Szemely vNev kNev elemD
        lista <- feldolgoz inf
        return (elemL : lista)
    
egySz = Szemely "Kelemen" "Bela" (Datum 8 17 1992)

--nevnap
{-keresF2 :: [[Char]] -> [Char] -> [Char]
keresF2 [] _ = ""
keresF2 (k: ve) mit
    | res == "" = keresF2 ve mit
    | otherwise = res
    where res = alkeres k mit
    
alkeres :: [Char] -> [Char] -> [Char]
alkeres [] [] = ""
alkeres [] _ = ""
alkeres l1 []
    | head l1 == ' ' = tail l1
    | otherwise = ""
alkeres l1 l2
    | head l1 /= head l2 = ""
    | otherwise = alkeres (tail l1) (tail l2)

my_words :: [Char] -> [[Char]]
my_words [] = []
my_words lista = k ++ ve
    where
    (kL, veL) = my_span (/= '\ n') lista
    k = if kL == "" then [] else [kL]
    ve = if veL == "" then [] else my_words (tail veL)
    
my_span :: (a -> Bool) -> [a] -> ([a], [a])
my_span _ [] = ([], [])
my_span fugv (k: ve)
    | fugv k = ((k:l1), l2)
    | otherwise = ([], (k:ve))
        where
        (l1, l2) = my_span fugv ve
        
mainF2 :: IO ()
mainF2 = do
    inf <- openFile "nevnapok.txt" ReadMode
    str <- hGetContents inf
    let miben = my_words str
    let res = keresF2 miben (knev egySz)
    putStrLn res
    hClose inf
    -}
