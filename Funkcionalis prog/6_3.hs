import Data.Char

type Hash = Integer
type Salt = String
type KDF = Salt -> String -> String

type Alphabet = [Char]

type Separator = Char
data Password = PSW Hash Salt
    deriving (Show)

jelszo = "MDALF"
main =  do
   let psw1 = renderPassword alphabet '$' (mkPassword kdf1 "toldi" jelszo)
   let psw2 = renderPassword alphabet '$' (mkPassword (kdf2 16) "toldi" jelszo)
   let psw3 = renderPassword alphabet '$' (mkPassword (kdf2 32) "toldi" jelszo)
   let psw4 = renderPassword alphabet '$' (mkPassword (kdf2 64) "toldi" jelszo)
   putStrLn psw1
   putStrLn psw2
   putStrLn psw3
   putStrLn psw4

renderPassword :: Alphabet -> Separator -> Password -> String
renderPassword abcStr sep psw = fLs
   where
   fLs =(numToWord abcStr ls) ++ [sep] ++ salt
   PSW ls salt = psw   

separator :: Separator
separator = '$'

alphabet :: Alphabet
alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

numToWord :: Alphabet -> Integer -> String
numToWord abcStr x = convert (toInteger(length abcStr)) x
   where
   convert b x
         | x < b = [fP x]
         | otherwise = (fP (rem x b): convert b (div x b))
   fP i = alphabet !! (fromIntegral i)

mkPassword :: KDF -> Salt -> String -> Password
mkPassword func s ls = PSW myHash mySalt
    where
    myHash = hash $ func s ls
    mySalt = s

hash :: String -> Hash
hash ls = hashAux ls 1
    where
    hashAux "" _ = 0
    hashAux (k:ve) p = temp + (hashAux ve p+1)
        where
        hatv = ord k
        temp =  p * (2 ^ hatv)

kdf1 :: KDF
kdf1 s ls = ls ++ s ++ (reverse ls)

kdf2 :: Int -> KDF
kdf2 x s ls =
    if s == "" then  take x (fS ls)
    else take x fLs
        where
            fLs = ls ++ (fS s)
            fS  s = s ++ (fS s)
