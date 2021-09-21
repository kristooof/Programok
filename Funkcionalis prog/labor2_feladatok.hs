--Labor2 feladatok
--I.
--1. szamjegyek szorzata
szorzat1 :: (Integral a) => a -> a
szorzat1 1 = 1
szorzat1 x = ( x `rem` 10 ) * szorzat1 ( x `div` 10 )

szorzat2 :: Int -> Int
szorzat2 x
    | x < 10 = x
    | x >= 10 = ( x `rem` 10 ) * szorzat2 ( x `div` 10 )

--2. szamjegyek osszege
osszeg1 :: Int -> Int
osszeg1 0 = 0
osszeg1 x = (x `rem` 10) + osszeg1 ( x `div` 10 )

osszeg2 :: Int -> Int -> Int
osszeg2 0 t = t
osszeg2 x t = osszeg2 (x `div` 10) (t + x `rem` 10)

--3 szamjegyek szama
szamjegyszam1 :: Int -> Int -> Int
szamjegyszam1 x i
    | x < 10 = i+1
    | x >= 10 = szamjegyszam1 (x `div` 10) (i+1)

szamjegyszam2 :: Int -> Int -> Int
szamjegyszam2 x i
    | x == 0 = i 
    | x > 0 = szamjegyszam2 (x `div` 10) (i+1)

--4 szamjegyek osszege parameter szerint
szl :: (Integral a) => a -> [a]
szl 0 = []
szl x = szl(x `div` 10) ++ [x `mod` 10]

addPair :: (Integral a) => [a] -> [Int] -> a
addPair xl [] = 0
addPair xl (lh:ls) = xl!!(lh-1)+ (addPair xl ls) 

suml :: (Integral a) => a -> [Int] -> a
suml x ls = addPair(szl x) ls

--5 paros szamjegyek szama
paros :: Int -> Int -> Int
paros x i
    | x == 0 = i
    | (( x `rem` 10 ) `rem` 2) == 0 = paros (x `div` 10) (i + x `rem` 10)
    
--6 egy szam legnagyobb szamjegye
lnszam :: Int -> Int-> Int
lnszam x i
    | (x < 10) && (x > i) = x
    | (x < 10) && (x < i) = i
    | (x >= 10) && (x `rem` 10 > i) = lnszam (x `div` 10) (x `rem` 10)
    | (x >= 10) && (x `rem` 10 < i) = lnszam (x `div` 10) i
    
--7 kettes szamrendszer beli alakjaban az egyesek szama
egyesek_szama x i 
    | x == 0 = i
    | x `mod` 2 == 1 = egyesek_szama (x `div` 2) (i+1)
    | x `mod` 2 == 0 = egyesek_szama (x `div` 2) i

--8 1000dik fibonacci szam. 100-nal mar nagyon lassu
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1) + fib (n-2))

--II.
fakt :: Int -> Int
fakt 0 = 1
fakt n = n * fakt (n-1)

fuggv = map (fakt) [0..10]
 
hatv :: (Integral a) => a -> a -> a
hatv x n
    | n < 0 = error "Negativ kitevo"
    | n == 0 = 1
    | mod n 2 == 0 = hatv (x*x) (div n 2)
    | otherwise = x * hatv (x*x) (div n 2)

fuggv2 :: (Integral a) => a -> [a]
fuggv2 x = map (hatv x) [0..10]

fuggv3 = map (osszeg1) [0..10]

--III.
--1. egy szam paros osztoinak listaja
parososzto :: Int -> [Int]
parososzto n = [ i | i <- [1..n], (n `mod` i == 0) && (i `mod` 2 == 0)] 

--2. az elso n term szam kobe
kob n = map (^3) [1..n]

kob2 n = [i^3 | i<-[1..n]] 

--3. az elso n term szam negyzetgyoke
gyok n = map (sqrt) [1..n]

gyok2 n = [sqrt(i) | i<-[1..n]]

--4. az n-nel kisebb pitagoraszi szamharmasok
pit n = [(a,b,c) | c<-[5..n], b<-[1..c], a<-[1..b], a^2+b^2==c^2]

--5. x hatvanyai n-ig
xhatv n x = [x^i | i<-[1..n], x^i<n]

--6. 
lista = parosit ['a'..'z'][0..25]
    where
        parosit (k1 : ve1) (k2 : ve2) = (k1, k2): parosit ve1 ve2
        
--7.
lista2 n = parosit2 [1..n](reverse [0..n])
    where
        parosit2 (k1:ve1) (k2:ve2) = (k1, k2) : parosit2 ve1 ve2