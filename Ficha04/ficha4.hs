import Data.Char

----------------
--1
----------------

--versao1
digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t) |isAlpha h = (ls,h:ds)
                 |isDigit h = (h:ls,ds)
                 |otherwise = (ls,ds)
        where (ls,ds) = digitAlpha t

---versao2(usando acumuladores)
digitAlphaAc :: String -> (String, String)
digitAlphaAc s = digitAlphaAc' ([],[]) s

digitAlphaAc' :: (String,String) -> String -> (String,String)
digitAlphaAc' ac [] = ac
digitAlphaAc' (ls,ds) (c:cs) | isDigit c = digitAlphaAc' (ls,c:ds) cs
                             | isAlpha c = digitAlphaAc' (c:ls,ds) cs
                             | otherwise = digitAlphaAc' (ls,ds) cs

----------------
--2
----------------

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | n < 0 = (n+1, z, p)
          | n == 0 = (n, z+1, p)
          |otherwise = (n, z, p+1)
        where (n,z,p) = nzp t

----------------
--3
----------------
divMod' :: Integral a => a -> a -> (a,a)
divMod' x y | (x-y) < 0 = (0,x)
            |otherwise = (q+1,r)
    where (q,r) = divMod' (x-y) y

----------------
--4
----------------

{-
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigitsAc :: [Int] -> Int
fromDigitsAc l = fromDigitsAc' 0 l


fromDigitsAc' :: Num a => a -> [a] -> [a]
fromDigitsAc' ac [] = []
fromDigitsAc' ac (h:t) = fromDigitsAc' (h+ac*10) t
-}

----------------
--9
----------------

--a)
e9a = [2^x | x <- [1..10]]

--b)
e9b = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6 ]

--c)
e9c = [[1..x] | x <- [1..5]]

--d)
e9d = [ replicate x 1 | x <- [1..5]]
--or
e9d' = [take n (repeat 1) | n <- [1 .. 5]] 

--e)
e9e = [ product [y | y <- [1..x]] | x <- [1..6]]