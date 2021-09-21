--I. Írjuk meg a beépített iterate, repeat, replicate, any, all függvényeket.
-- > take 10 (iterate (\x -> 2 * x ) 1)

-- > take 5 (repeat "hello")

-- > replicate 5 "hello"

-- > any isUpper "Sapientia University"

-- > all isUpper "Sapientia University"

-- II.
data Fesztivalok = Fesztivalok{
   egyuttes :: String,
   fesztival :: String,
   ar :: Int,
   kod :: Int
}deriving (Show) 
   
flista :: [Fesztivalok]
flista = [Fesztivalok "Karpatia" "EMI" 100 111,
           Fesztivalok "Bagossy" "EMI" 120 112,
           Fesztivalok "Hooligans" "Double Rise" 60 113,
           Fesztivalok "Follow the flow" "EMI" 50 114]
   
 -- 1. határozzuk meg egy adott fesztiválon szereplő eggyütteseket,
szerepel :: [Fesztivalok] -> String -> [Fesztivalok]
szerepel [] l = []
szerepel (k:ve) l
   | fesztival k == l = (k : szerepel ve l)
   | otherwise = szerepel ve l 
   
-- 2. határozzuk meg azokat az együtteseket, melyek egy adott értéknél olcsóbban árulják koncertjegyeiket,
olcso :: [Fesztivalok] -> Int -> [Fesztivalok]
olcso [] l = []
olcso (k:ve) l 
   | ar k < l = (k : olcso ve l)
   | otherwise = olcso ve l
   
-- 3. határozzuk meg, hogy hány olyan együttes szerepel a listában, mely egy adott értéknél olcsóbban árulja koncertjegyét,
olcsobb :: [Fesztivalok] -> Int -> Int -> Int 
olcsobb [] l n = n
olcsobb (k:ve) l n
   | ar k < l = olcsobb ve l (n+1)
   | otherwise = olcsobb ve l n
   
-- 4. rendezzük a lista tartalmát az együttesek nevei alapján ábécé sorrendbe (insertSort), használjuk a következő típusdefiníciót:
{- insertS :: [Fesztivalok] -> [Fesztivalok]
insertS [] = []
insertS (k: ve) = ins k (insertS ve) 
 where
    ins x [] = [x]
    ins x (k: ve)
        |egyuttes x > egyuttes k = (k: ins x ve)
        |otherwise = (x: (k: ve))-}
        
-- 5. rendezzük a lista tartalmát a jegyárak szerint csökkenő sorrendbe (qSort).
quickS :: [Fesztivalok] -> [Fesztivalok] 
quickS [] = []
quickS (k: ve) = quickS kLista ++ [k] ++ quickS nLista
    where
        kLista = [x | x <- ve, ar x > ar k]
        nLista = [x | x <- ve, ar x <= ar k]
        
-- 6. rendezzük a lista tartalmát összefésüléses rendezéssel a kod értékek alapján 
merge_sort :: [Fesztivalok] -> [Fesztivalok]
merge_sort ls
    | h <= 1 = ls
    | otherwise = my_merge (merge_sort e) (merge_sort v)
        where    
            h = length ls
            e = take (div h 2) ls
            v = drop (div h 2) ls

my_merge :: [Fesztivalok] -> [Fesztivalok] -> [Fesztivalok]       
my_merge [] [] =[]
my_merge [] ls = ls
my_merge ls [] = ls
my_merge (k1: v1) (k2: v2)
    | kod k1 < kod k2 = k1: (my_merge v1 (k2: v2))
    | otherwise = k2: (my_merge (k1: v1) v2)      

-- 7. határozzuk, meg, hogy agy adott fesztiválon mennyi a jegyek átlagértéke


atlag li n = div sum nr
    where
       (sum, nr) = satlag li n
       satlag [] n = (0, 0)
       satlag (k: ve) n
          |fesztival k == n = (ar k + temp1, 1+temp2)
          |otherwise = (temp1,temp2)
               where
                 (temp1, temp2) = satlag ve n

-- 8. írjuk meg az általános összefésüléses illetve beszúrásos rendezés algoritmusait
insertS :: [Fesztivalok] -> [Fesztivalok]
insertS [] = []
insertS (k: ve) = ins k (insertS ve)
 where
    ins x [] = [x]
    ins x (k: ve)
        |x >  k = (k: ins x ve)
        |otherwise = (x: (k: ve)) 
