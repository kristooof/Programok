import System.IO

movie[american_beauty, 1999]
director[american_beauty, sam_mendes]
actor[american_beauty, kevin_spacey, lester_burnham]
actress[american_beauty, annette_bening, carolyn_burnham]

	type Name = String
	type Role = String

fileProc :: Handle -> IO [Movie]
fileProc inf = do
	hEOF <- hIsEOF inf
	if hEOF then return []
	else do
		temp1 <- hGetLine inf
		let ls1 = words $ dropWhile (/= '[') temp1
		let n = init $ tail $ ls1 !! 0
		let y = read (init $ ls1 !! 1) :: Int
		temp2 <- hGetLine inf
		let ls2 = words $ dropWhile (/= '[') temp2
		let d = init $ ls2 !! 1
		(inf, act, ass) <- actorProc inf [] []
		let kM = Movie n y d act ass
		ve <- fileProc inf
		return (kM: ve)

data Movie = Movie{
	name :: String,
	year :: Int,
	director :: String,
	actor :: [(Name, Role)],
	actress :: [(Name, Role)]
}deriving (Show)


mainMovie = do
	inf <- openFile "movies.txt" ReadMode
	ls <- fileProc inf
	--putStrLn $ show ls
	--putStrLn $ show (length ls)
	hClose inf
	let lsM = listProc1 ls
	putStrLn $ show lsM