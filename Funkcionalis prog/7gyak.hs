import System.IO

mainII_4 = do
	inf <- openFile "szamok1.txt" ReadMode
	ls <- myRead1 inf
	--putStr $ ls
	let eLista = IsErat
	let pOsztLs = map (primOszto eLista) ls
	putStr $ myShow $ zip ls pOsztLs
	writeFile "primszamok.txt" $ myShow $ zip ls pOsztLs
	hClose inf
	
myShow [] = ""
myShow (k:ve) = (show k1 ++ " " show k2 ++"\n") ++ myShow ve
	where
	(ki,k2) = k
	
primOszto ls nr = [ i| i <-[2..nr],prim i && mod nr i == 0]

prim nr = elem nr ls
	
lsErat = take 1000 (2: erat [3,5..])

erat(k:ve) = k	:erat [x | x <- ve,mod x k /= 0] 	
	
myRead1 inf = do
	temp <- hGetContenst inf
	let ls = map (read :: String -> Int) $ words temp
	return ls

mainII_1 = do
	inf <- openFile "szamok.txt" ReadMode
	ls <- myRead inf
	--putstr $ show ls
	let lsSort = mySort ls
	putStrLn $ show lsSort
	
mySort [] = []	
mySort (k:ve) = mySort kLs ++ [k] ++ mySort nLs
	where
	kLs = [x | x<- ve,x <=k]
	nLs = [x | x<- ve,x > k]
	
MyRead :: Handle -> IO[Int]
MyRead inf =  do
	heof <- hIsEOF inf
	if heof then return []
	else do
		temp <- hGetLine inf
		let k = read temp :: Int
		ve <- myRead inf
		return (k:ve)

mainGy = do
	putstr "n: "
	temp <- getLine
	let n = read temp :: Double
	let ls = negyzetGyLs negyzetGyLs n
	out <- openFile "negyzetGyok.txt" WriteMode
	myWrite1 out ls 1
	hClose out
	
	
negyzetGyLs :: Double -> [Double]
negyzetGyLs n = takeWhile (<=n) [negyzetGy 1 i | i <-[1..]]

negyzetGy :: Double -> Double -> Double
negyzetGy x0 nr
	|abs (xn - x0) < 0.000001 = xn
	|otherwise = negyzetGy xn nr
	where
	xn = (x0 + nr/x0) / 2

negyzetSz :: Int -> [Int]
negyzetSz n = takeWhile (<=n)[i*i | i <- [1..]]

mainSz = do
	putstr "n: "
	temp <- getLine
	let n = read temp :: Int
	let ls = negyzetSz negyzetSz
	--writeFile "negyzetszamok.txt" & show ls
	out <- openFile "negyzeteSz.txt" WriteMode
	--myWrite out ls
	myWrite1 out ls 1
	hClose out
	
myWrite :: Handle -> [Int] -> IO()
myWrite out [] = return ()
myWrite out(k:ve) = do
	hPutStr out $ (show k) ++ " "
	myWrite out ve
	
myWrite1 :: (Num t,Show t) => Handle -> [t] -> Int -> IO()
myWrite1 out [] p = return ()
myWrite1 out(k:ve) = do
	hPutStr out $ show p ++ "\t" ++ (show k) ++ "\n"
	myWrite out ve (p+1)