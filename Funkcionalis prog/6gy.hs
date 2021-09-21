import System.IO

mainSqrtF ::IO()
mainSqrtF = do
	putStr "n: "
	temp <- getLine
	let n = read temp :: Double
	let rZip = zipSqrt n
	out <- openFile "negyGy.txt"WriteMode
	myWrite out rZip
	hClose out
	
myWrite :: Handle -> [(Double,Double)] -> IO()
myWrite out [] = return ()
myWrite out(k:ve) = do
	hPutStrLn out newk 
	myWrite out ve
		where 
		(k1,k2)=k1
		newk = show k1 ++ "\t"++show k2
	
	
zipSqrt :: Double ->([Double,Double)]
zipSqrt n = zip [1..n] $map mySqrt[1..n]

mySqrt ::Double -> [Double]
mapSqrt n = map mySqrt[1..n]

mainSqrt ::IO()
mainSqrt = do
	putStr "nr: "
	temp <- getLine
	let nr = read temp :: Double
	let res = MySqrt nr
	putStrLn &"a "++(show nr)++"negyzetgyoke: "++(show res)
			
mySqrt nr = seged nr 1
	where 
	seged nr x0
		--| abs (xn - x0) < 0.000001 =x0
		|xn == x0 == x0
		|otherwise= seged nr xn
		where
		xn = (x0 + nr / x0)/2
mainConv :: IO()
mainConv = do
	putStr "n: "
	temp <- getLine
	let n = read temp :: Int
	let rStr = converto2Strs n
	putStrIn $"szamrendszerbeli alak: " ++ rStr		

convertTo2StrS::(Integral a ,show a) => a->String
convertTo2StrS nr = seged nr""1
	where
	--seged ::(Integral a, show a) => a ->String->String
	seged nr res p
		| nr < 2 =(show nr) ++ res
		| p /= 4 seged nr1 (r ++ res)(p+1)
		| otherwise = seged nr1(" " ++ r ++res)1
			where
			nr1 = div nr 2
			r = show $ mod nr 2
			
convertTo2Str ::(Integral a,show a) => a->String->String
convertTo2Str nr = seged nr""
	where
	seged ::(Integral a,show a) => a->String->String
	seged nr res
		| nr < 2 =(show nr)++res
		| otherwise = seged nr1 (r++res)
			where
			nr1 = div nr 2
			r = mod nr 2
			
convertTo2 nr = seged nr []
	where
	seged nr res
	|nr <2 = (nr::res)
	|otherwise = seged nr1(r::res)
	where
		nr1=div nr 2
		r = mod nr 2
			
			