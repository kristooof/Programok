import Data.List(isPrefixOf)
import System.IO

data Datum = Datum{
	nap :: Int,
	honap:: Int,
	ev :: Int
	} deriving (Show)

data Szemely = Szemely {
	vnev :: [Char],
	knev :: [Char],
	szdatum :: Datum
	} deriving (Show)
	
data SzemelyR = SzemelyR {
	vnevR :: [Char],
	knevR :: [Char],
	szdatumR :: [Char]
	} deriving (Show)
	
	
	
fel10_2 = do
	inf <- openFile "szemelyR.txt" ReadMode
	--ls <- fileProcSt inf
	ls <- fileProcStR inf
	--putStrln $ show ls
	out <- openFile "szemelyConvR.txt"WriteMode
	hPutStrLn out $ myShow ls
	hclose inf
	hClose out

myShow [] = ""
myShow(k:ve) = newK ++ myShow ve
	where
	newK = (vnev k) ++ " "++(knevR k) ++ " " ++(szdatumR k)++"\n"
 

fileProcStR :: Handle -> IO [SzemelyR]
fileProcSt inf = do
	eof <- hIsEOF inf
	if eof then return []
	else do
		temp <- hGetLine inf
		let ls = words temp
		let vNev = ls !! 0
		let kNev = ls !! 1
		let nap = toRoman &(read (ls !! 2) :: Int)
		let ho = toRoman & (read(ls !! 3) :: Int)
		let ev = toRoman & (read(ls !! 4) :: Int)
		let k = SzemelyR vNevR kNevR (nap ++" "++ho++" "++ev)
		ve <- fileProcStR inf
		return (k : ve)	
		
fileProcSt :: Handle -> IO [Szemely]
fileProcSt inf = do
	eof <- hIsEOF inf
	if eof then return []
	else do
		temp <- hGetLine inf
		let ls = words temp
		let vNev = ls !! 0
		let kNev = ls !! 1
		let nap = (read (ls !! 2) :: Int)
		let honap = (read (ls !! 3) :: Int)
		let ev = (read (ls !! 4) :: Int)
		let kD = Datum nap honap ev
		let k = Szemely vNev kNev kD
		ve <- fileProcSt inf
		return (k : ve)
