import System.IO

type Name = String
type Role = String

data Movie = Movie{
    name :: String,
    year :: Int,
    director :: [Name],
    actor :: [(Name, Role)],
    actress :: [(Name, Role)]
}deriving (Show)


mainMovie = do
    inf <- openFile "movies.txt" ReadMode
    ls <- fileProc inf
    putStrLn $ show ls
    --putStrLn $ show (length ls)
    hClose inf


fileProc :: Handle -> IO [Movie]
fileProc inf = do
    hEOF <- hIsEOF inf
    if hEOF then return []
    else do
        temp1 <- hGetLine inf
        let ls1 = words $ dropWhile (/= '[') temp1
        let n = init $ tail $ ls1 !! 0
        let y = read (init $ ls1 !! 1) :: Int
        (inf, dir, act, ass) <- dataProc inf [] [] []
        --let k = Movie n y dir act ass
        let k = (n, y)
        ve <- fileProc inf
        return (k: ve)
        
putStr "kerek egy evszamot: "
    temp <- getLine
    let y = (read temp :: Int)
    let lsM = yearProc y ls
    if null lsM then putStrLn "nincsenek ilyen evbeli filmek"
       else do
       putStrLn temp ++ "a kiadott filmek: "
       putStrLn $ myShowshow lsM

mainMovie = do
   inf <- openFile "movies.txt" ReadMode
   ls <- fileProc inf
   hClose inf
   
   let mStr1 = "hail_caser"
   let mStr2 = "barton_fink"
   {-
   let res = movieProc mStr1 mStr2 ls
   if res == True then putStrLn "ugyanabban az evben keszultek"
   else putStrLn "nem ugyanabban keszultek / nem szeretep valamelyik film"
   -}
   
   let res1 = myFind_mStr1 ls
   let res2 = myFind_mStr2 ls
   if null res1 || null res2 || res1 /= res2 then 
      putStrLn "nem ugyanabban keszultek / nem szerepel valamelyik film"
   else
      putStrLn "ugyanabban az evben keszultek" 

myFind_ :: Int -> [Movie] -> [Int]      
myFind_x ls = [year k | k <- ls,x==name k]

movieProc :: String -> String -> [Movie] -> Bool
movieProc mStr1 mStr2 ls = 
   if res1 == -1 || res2 == -1 then False
   else
      if res1 == res2 then True
      else False
      where
      res1 = myFind mStr1 ls
      res2 = myFind mStr2 ls

myFind :: String -> [Movie] -> Int
myFind x [] = -1
myFind x (k:ve)
   | x == name k = year k
   | othetwise = myFind x ve

--myFind_x  ls = [year k  | k <- ls, x == name k]

myShow [] = ""
myShow (k:ve) = newK ++ myShow ve
   where
   (k1,k2) = k1
   --newK = l1 ++ " " ++ show k2 ++ "\n"
   newK = k1 ++ "\n"

mainMovie2 = do
   inf <- openFile "movies.txt" ReadMode
   ls <- fileProc inf
   hClose inf
   
   let aLs = setActorList $ actorList ls
   
   out <- openFile "actress.txt" WriteMode
   hPutStrLn out $ myShowA aLs
   hClose out

actorList :: [Movie] -> [Name]
actorList [] = []
actorList (k: ve) = lsAct ++ lsAss ++ actorList ve
   where
     lsAct = myFst $ actor k
     lsAss = myFst $ actress k
     myFst [] = []
     myFst ((k1, k2): ve) = k1: myFst ve
     
setActorList :: Eq t => [t] -> [t]
setActorList [] = []
setActorList (k: ve) = (k: setActorList [x | x <- ve, k /= x])

myShowA :: [[Char]] -> [Char]
myShowA [] = ""
myShowA (k: ve) = k ++ "\n" ++ myShowA ve

--yearProc :: Int -> [Movie] -> [String]
yearProc _ [] = []
yearProc y(k:ve)
   |tY == y(name k,director k):yearProc 
   |othetwise = yearProc y ve
      where
      tY = year k
        
dataProc :: Handle -> [String] -> [(String, String)] -> [(String, String)] -> IO (Handle, [String], [(String, String)], [(String, String)])
dataProc inf ds ls1 ls2 = do
    temp <- hGetLine inf
    let nTemp = takeWhile (/='[') temp
    case nTemp of
        " " -> return (inf, ds, ls1, ls2)
        "director" -> do
            let als = words $ dropWhile (/='[') temp
            let dn = init $ als !! 1
            dataProc inf (dn: ds) ls1 ls2
        "actor" -> do
            let als = words $ dropWhile (/='[') temp
            let an = init $ als !! 1
            let rn = init $ als !! 2
            dataProc inf ds ((an, rn): ls1) ls2
        "actress" -> do
            let als = words $ dropWhile (/='[') temp
            let an = init $ als !! 1
            let rn = init $ als !! 2
            dataProc inf ds ls1 ((an, rn): ls2)
