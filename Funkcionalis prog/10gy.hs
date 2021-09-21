import Data.List (isPrefixOf)

fel10_1 = do
	rom1 <- getLine
	rom2 <- getLine
	let nr1 = toArabic1 rom1
	let nr2 = toArabic1 rom2
	let res =  toRoman (nr1 + nr2)
	putStrLn "Osszeg: "
	putStrLn & (show nr1)(++ " "++(show nr2)++" "++(show $ nr1 + nr2)
	putStrLn res
	
	let res = nr1 - nr2
	putStrLn "Kulonbseg: "
	putStrLn & (show nr1)(++ " "++(show nr2)++" "++(show $ nr1 - nr2)
	putStrLn & toRoman res

romArab = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]

toArabic1 :: String -> Int
toArabic1 "" = 0
toArabic1 str = nr + toArabic1 tStr
	where
	(nr, tStr): _ = [ (nr, drop (length tStr) str) | (tStr, nr) <- romArab, isPrefixOf tStr str ]

toRoman :: Int -> [Char]
toRoman nr
	|nr>=0=auxRoman nr0
	|otherwise ="-" ++auxRoman(abs nr) 0
		where
		auxRoman :: Int -> Int -> [Char]
		auxRoman nr p
			| nr <= 0 = ""
			| otherwise = auxRoman (div nr 10) (p+1) ++ (toRomanDigit p (rem nr 10))

toRomanDigit :: Int -> Int -> [Char]
toRomanDigit p d =
	case p of
	0 -> toROne 'I' 'V' 'X' d
	1 -> toROne 'X' 'L' 'C' d
	2 -> toROne 'C' 'D' 'M' d
	3 -> toRTh 'M' d
	_ -> error "nagy romai szam"
	
toRTh :: Char -> Int -> [Char]
toRTh sz d = replicate d sz

toROne :: Char -> Char -> Char -> Int -> [Char]
toROne sz1 sz2 sz3 d
	| d < 4 = replicate d sz1
	| d == 4 = [sz1] ++ [sz2]
	| d == 5 = [sz2]
	| d == 9 = [sz1] ++ [sz3]
	| otherwise = [sz2] ++ (replicate (d - 5) sz1)
