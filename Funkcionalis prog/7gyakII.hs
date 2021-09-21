import System.IO
import Control.Exception

my)maxR ls = foldr func(head ls) (tail ls)
	where
	func x y if x > y then x else y

my)maxL ls = foldl max (head ls) (tail ls)

my_reverseR :: [a] -> [a]
my_reverseR ls = foldlr(\k res -> res ++[k)[] ls

my_reverseR :: [a] -> [a]
my_reverseL ls= foldl(\res k -> (k:res)) [] ls

feladat8_II_1 = do
	putStr "n: "
	temp <- getLine
	let n = read temp :: Int
	putStr "m: "
	temp <- getLine
	let m - read temp :: Int
	let abc  = take n['A'..'Z']
	let lexi = lexi abc m
	putStrLn $ show res
	--putStrLn $ show res
	out <- openFile "abc.txt" WriteMode
	hPutStr out $ myshow res
	hclose out
		where 
		myShow [] = ""
		myShow(k:ve) = k ++ "\n" ++myShow ve

lexi ls 0 = [()]
lexi ls m = [(k:ve) | k <- ls,ve <- lexi ls(m-1)]

mainIII_1 = catch (do
	inf1 <- openBinaryFile "kep2.jpg" ReadMode
	inf2 <- openBinaryFile "kep0.jpg" ReadMode
	bStr1 <- hGetContents inf1
	bStr2 <- hGetContents inf2
	--bInd <- compPoz bSr1 bStr2(-1)
	let bInd = compPoz bStr1 bStr2 (-1)
	putStrLn $ "poziciok: " ++ show bInd
	hClose inf1
	hClose inf2
	) kivetelF
		where 
		kivetelF :: SomeException -> IO()
		kivetelF err = putStrLn $ "Hiba: " ++ show err
{-	
compPoz [] [] _ = return[]
compPoz (k1: ve1) (k2: ve2) p = do
	if k1 /= k2 then do
		ve <- compPoz ve1 ve2(p+1)
		return (p+1:ve)
	else do
		ve <- compPoz ve1 ve2(p+1)
		return ve
		-}
		
compPoz [] [] _ = []
compPoz [] ve _ = [-1]
compPoz ve [] _ = [-1]
compPoz (k1:ve1) (k2:ve2) p
	| k1 /= k2 = (p+1: ve)
	|otherwise = ve
		where
		ve = compPoz ve1 ve2 (p+1)
		