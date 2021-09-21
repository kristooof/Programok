--Labor 4 feladatok
--I. 
--my_maximum :: (Ord a) => [a] -> a
my_maximum [] = error "ures lista"
my_maximum [k] = k
my_maximum (k:ve)
    | k > maxTail = k
    | otherwise = maxTail
    where maxTail = my_maximum ve

{-maxElemPoz (k:ve) ls i 
	| k== (my_maximum (k:ve)) = ls++[i]
	| otherwise = maxElemPoz ve ls i+1-}
    
maxElemPoz2 :: (Ord a) => [a] -> (a, [Int])
maxElemPoz2 [] = error "Ures lista"
maxElemPoz2 (k: ve) = smaxPoz k ve 1 [0]
    where
    smaxPoz :: (Ord a) => a -> [a] -> Int -> [Int] -> (a, [Int])
    smaxPoz m [] ind res = (m, res)
    smaxPoz m (k: ve) ind res
        | m == k = smaxPoz m ve (ind + 1) (ind: res)
        | m > k = smaxPoz m ve (ind + 1) res
        | m < k = smaxPoz k ve (ind + 1) [ind]

--Polinom adott ertekre valo behelyettesitesepoli :: [Integer] -> Integer -> Integer ->Integer
polinom [] x0 seged = error "Nincsenek egyutthatok!"
polinom [k] x0 seged = k * seged 
polinom (k: ve) x0 seged = polinom ve x0 (seged*x0) + k * seged

--alakitas 10-es szamrendszerbe p szamrendszerbol
alakit10be :: [Integer] -> Integer -> Integer ->Integer
alakit10be [] p seged = error "Nincs megadva szam"
alakit10be [k] p seged = k*seged 
alakit10be (k:ve) p seged = alakit10be ve p seged*p + k*seged

--II.
my_eleml x ls = foldl (\y1 y2 -> if y2 == x then True else y1) False ls 

my_productl ls= foldl (\x y -> x * y) 1 ls

my_elemr x ls = foldr (\y1 y2 -> if y1 == x then True else y2) False ls

my_productr ls = foldr (\x y -> x * y) 1 ls

--horner :: (Num a) => a -> [a] -> a
horner x ls = foldr (\a b -> a + b*x) ls

--III.
data Fesztivalok = 
 Fesztivalok {  egyuttes :: String,
                fesztival :: String,
                ar :: Int,
                kod :: Int } deriving (Show, Eq)
                
fesztival_lista :: [Fesztivalok]
fesztival_lista = [ Fesztivalok "Road" "EMI" 50 1,
                    Fesztivalok "Pokolgep" "EMI" 100 2,
                    Fesztivalok "Zanzibar" "Rockmaraton" 75 3,
                    Fesztivalok "P-mobil" "KAROCK" 210 4]

--a.
szerepelnek :: String -> [Fesztivalok] -> [String]
szerepelnek e [] = []
szerepelnek e (x : xs)
    | e == fesztival x = ( egyuttes x: szerepelnek e xs )
    | otherwise = szerepelnek e xs

--b.
olcsobb_jegyek :: Int -> [Fesztivalok] -> [String]
olcsobb_jegyek jegyar [] = []
olcsobb_jegyek jegyar (x:xs)
    | ar x < jegyar = ( egyuttes x : olcsobb_jegyek jegyar xs )
    | otherwise = olcsobb_jegyek jegyar xs
                    
--c.
olcsobb_jegyek_szama :: Int -> [Fesztivalok] -> Int -> Int 
olcsobb_jegyek_szama jegyar [] i = i
olcsobb_jegyek_szama jegyar (x:xs) i
    | ar x < jegyar = olcsobb_jegyek_szama jegyar xs i+1
    | otherwise = olcsobb_jegyek_szama jegyar xs i
    
--d.
    
ins  :: (a -> a -> Bool) -> a -> [a] -> [a]
ins fc x [] = [x]
ins fc x (k: ve)
    | fc x k = (k: ins fc x ve)
    | otherwise = (x: (k: ve))

insertS :: (a -> a -> Bool) -> [a] -> [a]
insertS fc [] = []
insertS fc (k: ve) = ins fc k (insertS fc ve)


compSz :: Fesztivalok -> Fesztivalok -> Bool
compSz x y = egyuttes x < egyuttes y
--meghivas: insertS compSz fesztival_lista

--e.
compAr :: Fesztivalok -> Fesztivalok -> Bool
compAr x y = ar x < ar y
--meghivas: insertS compAr fesztival_lista

--f.
atlag_jegyar :: String -> [Fesztivalok] -> Double
atlag_jegyar feszt li = (fromIntegral sum) / (fromIntegral nr)
    where 
    (sum, nr) = satlag feszt li
    satlag :: String -> [Fesztivalok] -> (Int, Int) 
    satlag feszt [] = (0, 0)
    satlag feszt (x:xs) 
        | fesztival x == feszt = (ar x + temp1, 1 + temp2)
        | otherwise = (temp1, temp2)
        where
        (temp1,temp2) = satlag feszt xs
        
--IV.
hatv0 :: Integer -> Integer -> Integer
hatv0 x n
    | n < 0 = error "negativ kitevo"
    | n == 0 = 1
    | otherwise = x * hatv0 x (n-1)

hatv1 :: Integer -> Integer -> Integer
hatv1 x n
    | n < 0 = error "negativ kitevo"
    | n == 0 = 1
    | mod n 2 == 1 = x* hatv1 (x*x) (div n 2)
    | otherwise = hatv1 (x*x) (div n 2)

hatv2 :: Integer -> Integer -> Integer
hatv2 x n
    | n < 0 = error "negativ kitevo"
    | n == 0 = 1
    | mod n 2 == 1 = x * h * h
    | otherwise = h * h
        where h = hatv2 x (div n 2)
-- Az ido mereset a :set +s idomerovel allitottam be
-- 10000-et emelve a 10000-dik hatvanyra es a 10000-et a 100000-dik hatvanyra emeltem
-- ido alapjan  mindket esetben a hatv1 valtozat lett a leggyorsabb, es a hatv0 valtozat a leglassubb
