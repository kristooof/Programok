import System.IO
data Film = Film{
    cim :: String,
    rend :: String,
    ev :: Int,
    kolt :: Int
    } deriving (Show)
    
mainF = do
   fIn <- openFile "film.txt" ReadMode
   lsF <- fileProc fIn
   let lsQ = qSortF lsF
   putStrLn $ myShow lsQ
   let v = (length lsQ) - 1
   let poz = bSearch lsQ 2015 0 v
   if poz < 0 then
    putStrLn "nincs benne" 
   else do
      putStrLn "A keresett film: "
      --putStrLn & show (lsQ !! poz)
      let lsF1 = myFilter1 lsQ (poz - 1)v filmEv
      let lsF2 = myFilter2 lsQ (poz + 1)v filmEv
      putStr $ myShow lsF1
      putStrLn $ show (lsQ !! poz)
      putStrLn $ myShow lsF2
      
   hClose fIn

myFilter1 ::[Film] -> Int -> Int ->[Film]
myFilter1 ls p v aEv
    |p < 0 = []
    |aEv == temp = (ls !! p): myFilter1 ls (p-1)v aEv
    |otherwise = []
    where
    temp = ev (ls !! p)
  
myFilter2 :: [Film] -> Int -> Int -> Int -> [Film]  
myFilter2 ls p v aEv
    |p>v =  []
    |aEv == temp = (ls !! p): myFilter2 ls (p-1) aEv
    |otherwise = []
    where
    temp = ev (ls !! p)

bSearch :: [Film] -> Int -> Int -> Int -> Int -- a poziciot hatarozzuk meg   
bSearch ls elem k v 
    | k > v = -1
    | aK == elem = i
    | aK > elem = bSearch ls elem k (i-1)
    | aK < elem = bSearch ls elem (i+1) v
       where 
        i = div (k + v) 2
        aK = ev (ls !! i)
        
   
myShow :: [Film] -> String
myShow [] = ""
myShow (k:ve) = (show k) ++ "\n" ++ myShow ve

--rendezes megjeleneesi ev szerint novekvo sorrend
qSortF :: [Film] -> [Film]
qSortF [] = []
qSortF (k:ve) = kLista ++ [k] ++ vLista
    where 
     kLista = qSortF[x | x <- ve, ev x < ev k]
     vLista = qSortF[x | x <- ve, ev x >= ev k]
 
--fileProcT :: Handle -> IO[String]
fileProcT fIn = do
    eof <- hIsEOF fIn
    if eof then return []
    else do
        temp <- hGetLine fIn
        let ls = words temp 
        -- let c = ls !! 0
        let c = ls !! 1
        --let c = (read (ls !! 2) :: Int)
        --let c=(read (ls !! 3) ::Int)
        ve <- fileProcT fIn
        return (c:ve)
        
fileProc :: Handle -> IO[Film]
fileProc fIn = do
    eof <- hIsEOF fIn
    if eof then return []
    else do
        temp <- hGetLine fIn
        let ls = words temp 
        let c = ls !! 0
        let r = ls !! 1
        let e = (read (ls !! 2) ::Int)
        let o = (read (ls !! 3) :: Int)
        let kF = Film c r e o
        ve <- fileProc fIn
        return (kF:ve)