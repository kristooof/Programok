--Labor 6 feladatai
--I.
--1. 
import Data.List

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

olvasNInt :: Int -> IO [Int]
olvasNInt n = do
    k <- getInt
    if n == 1 then return [k]
    else do
        ve <- olvasNInt (n-1)
        return (k: ve)

hanyszor_szerepel n [] i = i
hanyszor_szerepel n (k:ve) i 
    | n == k = hanyszor_szerepel n ve (i+1)
    | otherwise = hanyszor_szerepel n ve i
    
feladatI_1 n k = do 
    putStr "tomb elemei: "
    ls <- olvasNInt n
    putStrLn (show ls)
    let i = (hanyszor_szerepel k ls 0)
    putStrLn (show i)
    
--I.2
elemenkent [] = []
elemenkent (k:ve) = [(k, k1)] ++ (elemenkent ve)
    where 
        k1 = hanyszor_szerepel k (k:ve) 0

feladatI_2 n = do   
    putStr "tomb elemei: "
    ls <- olvasNInt n
    putStrLn (show ls)
    let i = elemenkent ls
    putStrLn (show i)

--I.3

feladatI_3 n = do
    putStr "tomb elemei: "
    ls <- olvasNInt n
    putStrLn (show ls)
    let s = fromIntegral (sum ls) / (fromIntegral (length ls)) 
    putStrLn (show s)

--II.1
feladatII_1 = do
    putStrLn "elso szam: "
    sz1 <- getInt
    putStrLn "masodik szam: "
    sz2 <- getInt
    let i = sum [sz1..sz2]
    putStrLn (show i)
    
--II.2
eszita :: Int -> [Int]
eszita n = takeWhile ((>)n) (2: szita [3, 5..])

szita :: [Int] -> [Int]
szita (k:ve) = (k: szita [x | x <- ve, mod x k /= 0])

primek n m = dropWhile ((>)n) (eszita m)

feladatII_2 = do
    putStrLn "elso szam: "
    sz1 <- getInt
    putStrLn "masodik szam: "
    sz2 <- getInt
    let ls = primek sz1 sz2
    let ossz = sum ls
    putStrLn (show ossz)

--II.3
osztok n = [i|i<-[1..n], n `mod` i ==0]

legnOsztok a b
    | length(osztok a) > length(osztok b) = a
    | otherwise = b

feladatII_3 = do
    putStrLn "elso szam: "
    sz1 <- getInt
    putStrLn "masodik szam: "
    sz2 <- getInt
    let nagyobb = legnOsztok sz1 sz2
    putStrLn (show nagyobb)

--III.
--1.
gyok 0 n = 1.0
gyok a n = (prev + fromInteger(n)/prev)/2.0
    where
        prev = gyok (a-1) n

lista n = map (gyok (n*2)) [1..(n-1)]

feladatIII_1 :: IO()
feladatIII_1 = do
    putStrLn "szam: "
    sz1 <- getLine
    let n = read sz1 :: Integer
    let gyokok = lista n
    writeFile "gyok.txt" (show gyokok)

--2.
kobgyok 0 n = 1.0
kobgyok a n = ((2*elozo2) + fromInteger(n)/ (elozo2 *elozo2 )) / 3.0
    where
        elozo2 = kobgyok (a-1) n

lista2 n = map (kobgyok (n*3)) [1..(n-1)]

feladatIII_2 :: IO()
feladatIII_2 = do
    putStrLn "szam: "
    sz1 <- getLine
    let n = read sz1 :: Integer
    let gyokok = lista2 n
    writeFile "kobgyok.txt" (show gyokok)

--3.
negyzetszamok n = map (^2) [1..(n-1) `div` 2]

feladatIII_3 :: IO()
feladatIII_3 = do
    putStrLn "szam: "
    sz1 <- getInt
    let negyzetek = negyzetszamok sz1
    writeFile "negyzetszamok.txt" (show negyzetek)