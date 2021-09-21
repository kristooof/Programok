--Labor 7 feladatok
import System.IO
import Data.Char
import Data.List
import Data.Bits
--I-1.  
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] li = li
merge li [] = li
merge (k1: ve1) (k2: ve2)
    | k1 < k2 = (k1: merge ve1 (k2: ve2))
    | otherwise = (k2: merge (k1: ve1) ve2)

mergeS :: (Ord a) => [a] -> [a]
mergeS [] = []
mergeS [k] = [k]
mergeS li = merge bLista jLista
    where
    db = div (length li) 2
    bLista = mergeS (take db li)
    jLista = mergeS (drop db li)

olvas :: IO()
olvas = do
    inf <- openFile "szamok.txt" ReadMode
    outf <- openFile "szamokRendezett.txt" WriteMode
    ln <- hGetLine inf
    let lista = read ln::[Int]
    let lista2 = mergeS lista
    hPutStr outf (show lista2)
    hClose inf
    hClose outf

--I.2
convert :: Int -> Int -> [Int]
convert nr p = sConvert nr p []
    where
    sConvert nr p res
        | nr < p = (nr: res)
        | otherwise = sConvert (div nr p) p (mod nr p: res)

hanyegyes nr = sum (convert nr 2)

alakit [] = []
alakit (k:ve) = [(k, k1, o)] ++ (alakit ve)
    where 
        k1 = convert k 2
        o = hanyegyes k

olvas2 :: IO()
olvas2 = do
    inf <- openFile "szamok.txt" ReadMode
    outf <- openFile "szamokAlakitva.txt" WriteMode
    ln <- hGetLine inf
    let lista = read ln::[Int]
    let lista2 = alakit lista
    hPutStr outf (show lista2)
    hClose inf
    hClose outf

--I.3
szamrendszerek [] = []
szamrendszerek (k: ve) = [(k, k2, k16, k256)] ++ (szamrendszerek ve)
    where 
        k2 = convert k 2
        k16 = convert k 16
        k256 = convert k 256

olvas3 :: IO()
olvas3 = do
    inf <- openFile "szamok.txt" ReadMode
    outf <- openFile "szamokSzamrendszerben.txt" WriteMode
    ln <- hGetLine inf
    let lista = read ln::[Int]
    let lista2 = szamrendszerek lista
    hPutStr outf (show lista2)
    hClose inf
    hClose outf 

--I.4
osztok nr = [i | i<-[1..nr], nr `mod` i == 0]

eszita :: Int -> [Int]
eszita n = takeWhile ((>) n) (2: szita2 [3, 5 ..])

szita2 :: [Int] -> [Int]
szita2 (k: ve) = (k: szita2 [x | x <- ve, mod x k /= 0])

kozosElemek [] primek = []
kozosElemek (k1:ve1) primek
    | elem k1 primek = k1 : kozosElemek ve1 primek
    | otherwise = kozosElemek ve1 primek

kialakit [] = []
kialakit (k:ve) = [(k, k1)] ++ (kialakit ve)
    where
        k1 = kozosElemek (osztok k) (eszita k)

olvas4 :: IO()
olvas4 = do
    inf <- openFile "szamok.txt" ReadMode
    outf <- openFile "szamokPrimosztoi.txt" WriteMode
    ln <- hGetLine inf
    let lista = read ln::[Int]
    let lista2 = kialakit lista
    hPutStr outf (show lista2)
    hClose inf
    hClose outf

--II.1
mainCmpI :: IO ()
mainCmpI = do
    inf1 <- openBinaryFile "kep.jpg" ReadMode
    inf2 <- openBinaryFile "nkep.jpg" ReadMode
    bInd <- byteCmp inf1 inf2 0 0
    if bInd == 0 then
        putStrLn "OK, identical files"
    else do
        putStr "different files on poz: "
        putStrLn $ show bInd
    hClose inf1
    hClose inf2

byteCmp :: Num a => Handle -> Handle -> a -> a -> IO a
byteCmp inf1 inf2 bInd bInd1 = do
    heof1 <- hIsEOF inf1
    heof2 <- hIsEOF inf2
    if heof1 && heof2 then return bInd
    else do
        if heof1 || heof2 then return bInd1
        else do
            nr1 <- hGetChar inf1
            nr2 <- hGetChar inf2
            if nr1 /= nr2 then return bInd1
            else
                byteCmp inf1 inf2 bInd (bInd1 + 1)

--II.2
mainF2 :: IO ()
mainF2 = do
    putStr "file name: "
    name <- getLine
    putStr "Byte : "
    tbyteS <- getLine
    let bMit = map hexToByte (words tbyteS)
    inf <- openBinaryFile name ReadMode
    bMiben <- hGetContents inf
    let res = find2 bMiben bMit
    putStr "result: "
    putStrLn $ show res
    hClose inf

hexToByte :: [Char] -> Char
hexToByte [c] = chr (hexChar c)
hexToByte [c1, c2] = chr ((hexChar c1) * 16 + (hexChar c2))
hexToByte _ = error "Hiba"

hexChar :: Char -> Int
hexChar c
    | c <= '9' && c >= '0' = (ord c) - 48
    | c == 'a' || c == 'A' = 10
    | c == 'b' || c == 'B' = 11
    | c == 'c' || c == 'C' = 12
    | c == 'd' || c == 'D' = 13
    | c == 'e' || c == 'E' = 14
    | c == 'f' || c == 'F' = 15
    | otherwise = error "Hiba"

find2 :: [Char] -> [Char] -> Bool
find2 [] byteS = False
find2 list byteS
    | sFind list byteS = True
    | otherwise = find2 (tail list) byteS
    where
    sFind [] ue = False
    sFind _ [] = True
    sFind (k: ve) (l: ue)
        | k /= l = False
        | otherwise = sFind ve ue

--II.3
mainGetFSize :: IO ()
mainGetFSize = do
    putStr "file name: "
    name <- getLine
    inf <- openBinaryFile name ReadMode
    size <- hFileSize inf
    putStrLn "fsize: "
    putStrLn (show (fromIntegral size))
    hClose inf

--II.4
mainCmpII :: IO ()
mainCmpII = do
    putStr "Elso file: "
    name1 <- getLine
    putStr "Masodik file: "
    name2 <- getLine
    inf1 <- openBinaryFile name1 ReadMode
    inf2 <- openBinaryFile name2 ReadMode
    bInd <- byteCmp inf1 inf2 0 0
    if bInd == 0 then
        putStrLn "OK, azonos fajlok"
    else do
        putStr "Kulonbozo fajlok ezen a pozicion: "
        putStrLn $ show bInd
    hClose inf1
    hClose inf2

--II.5
mainCpyI :: IO ()
mainCpyI = do
    putStr "A fajl, melyrol masolatot keszitunk: "
    name <- getLine
    putStr "A masolat neve: "
    name2 <- getLine
    inf <- openBinaryFile name ReadMode
    outf <- openBinaryFile name2 WriteMode
    blista <- byteIn inf
    byteOut outf blista
    hClose inf
    hClose outf

byteIn :: Handle -> IO [Char]
byteIn inf = do
    heof <- hIsEOF inf
    if heof then return []
    else do
        nr <- hGetChar inf
        nLista <- byteIn inf
        return (nr: nLista)

byteOut :: Handle -> [Char] -> IO ()
byteOut out [] = return ()
byteOut outf (k: ve) = do
    hPutChar outf k
    byteOut outf ve
    
--II.6
felII_6 :: IO ()
felII_6 = do
    putStr "A fajl neve: "
    name <- getLine
    putStr "A titkositas eredmenye: "
    name2 <- getLine
    inf <- openFile name ReadMode
    outf <- openFile name2 WriteMode
    kulcsok <- openFile "kulcsok.txt" ReadMode
    kulcs <- hGetLine kulcsok
    sorok <- hGetLine inf
    let kl= read kulcs::[Char]
    let kulcs_lista = map fromEnum (cycle kl)
    let tl = read sorok::[Char]
    let titkositani_lista = map fromEnum tl
    let titkos = map toEnum (zipWith xor kulcs_lista (map fromEnum titkositani_lista))
    byteOut outf titkos
    hClose inf
    hClose outf
    