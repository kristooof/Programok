--Labor3 feladatok
--I.
--1. 
my_last :: [a] -> a
my_last [] = error "ures lista"
my_last [k] = k
my_last (k: ve) = my_last ve

--2.
my_init :: [a] -> [a]
my_init [] = error "ures lista"
my_init [k] = []
my_init (k: ve) = (k: my_init ve)

--3.
my_maximum :: (Ord a) => [a] -> a
my_maximum [] = error "ures lista"
my_maximum [k] = k
my_maximum (k:ve)
    | k > maxTail = k
    | otherwise = maxTail
    where maxTail = my_maximum ve

--4.
my_minimum :: (Ord a) => [a] -> a
my_minimum [] = error "ures lista"
my_minimum [k] = k
my_minimum (k:ve)
    | k < minTail = k
    | otherwise = minTail
    where minTail = my_minimum ve

--5.
--my_sum :: [Integer] -> Integer
my_sum [] = 0
my_sum (k: ve) = k + (my_sum ve)

my_length :: [Integer] -> Integer
my_length [] = 0
my_length (k: ve) = 1 + my_length ve

my_avg :: [Integer] -> Float
my_avg (k: ve) = (fromInteger (my_sum (k: ve))) /  (fromInteger (my_length (k: ve)))


--6.
palindrome ls 
    | ls == reverse ls = True
    | otherwise = False


--7
--alakitszamrendsz a -> a -> [a] -> [a]
alakitszamrendsz 0 p ls = ls
alakitszamrendsz x 0 ls = error "hibas szamrendszer"
alakitszamrendsz x p ls =  alakitszamrendsz (x `div` p) p ((x `mod` p) : ls)

--lsz :: [a] -> a -> a
lsz [] a = a
lsz ls a = lsz (tail ls) (a*10 + (head ls))

alakit x p a= lsz (alakitszamrendsz x p []) a


--8 szamjegyek szama a lista minden elemere
--szamjegyszam :: Int -> Int -> Int
szamjegyszam i 0 = i
szamjegyszam i x = szamjegyszam (i+1) (x `div` 10)

szamjegyli ls i = map (szamjegyszam i) ls

--9 ossz szamjegyszam
ossz ls i = my_sum (szamjegyli ls i)

--II.
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (k: ve) = (reverse1 ve) ++ [k]
-- 9-10 s

reverse2 :: [a] -> [a] 
reverse2 li = sreverse2 li []
    where
        sreverse2 [] li = li
        sreverse2 (k: ve) li = sreverse2 ve (k: li)
--4-5 s // sokkal gyorsabban mukodik segedfuggveny hasznalataval