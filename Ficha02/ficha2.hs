import Data.Char ( ord, isDigit, isLower )
----------------
--2
----------------

--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros l = map (*2) l

--b)
numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a str = length $ filter (==a) str

--c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h < 0 = False
                | otherwise = positivos t

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos l = filter (>=0) l

--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg l = somaL $ filter (<0) l

somaL :: [Int] -> Int
somaL [] = 0
somaL (h:t) = h + (somaL t)

--f)
tresUlt :: [a] -> [a]
tresUlt l | (length l) < 4 = l
          | otherwise = take 3 (reverse l)

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t


--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((x,y):t) | a == x = True
                         | otherwise = nosPrimeiros a t

--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):ts) = (x + sumX, y + sumY, z + sumZ)
        where (sumX, sumY, sumZ) = sumTriplos ts

----------------
--3
----------------

--a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h  = h : soDigitos t
                | otherwise = soDigitos t

--b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas l = length(filter(isLower) l)

--c)
nums :: String -> [Int]
nums [] = []
nums' str = [ord num - ord '0' | num <- str, elem num ['0'..'9']]
----------------
--4
----------------
type Polinomio = [Monomio]
type Monomio = (Float, Int)

--a)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((a,b):t) |n == b = 1 + conta n t
                  |otherwise = conta n t

--b)
grau :: Polinomio -> Int
grau [] = 0
grau ((a1,b1):s) = max b1 (grau s)

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((a,b):t) |n == b = (a,b) : selgrau n t
                    |otherwise = selgrau n t

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) |b > 0 = (a*fromIntegral b,b-1):deriv t
                |otherwise = deriv t

--e)
calcula :: Float -> Polinomio -> Float
calcula 0 _ = 0
calcula _ [] = 0
calcula n ((a,b):t) = (a*n)^b + calcula n t

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) |b == 0 = simp t
               |otherwise = (a,b): simp t

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a1,b1) ((a2,b2):t) = (a1*a2,b1+b2) : mult (a1,b1) t

--h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(a,b)] = [(a,b)]
normaliza ((a1,b1):(a2,b2):t)|b1 == b2 = normaliza ((a1+a2,b1):t)
                             |conta b1 t == 0 = (a1,b1) : normaliza ((a2,b2):t)
                             |otherwise = normaliza ((a1,b1):t ++ [(a2,b2)]) 

--i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza(p1 ++ p2)

--f)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p2 = soma(mult h p2) (produto t p2)

--k)
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a1,b1):(a2,b2):t) | b2 > b1 = (a2,b2): ordenaM (a1,b1) t

ordenaM :: Monomio -> Polinomio -> Polinomio
ordenaM (a,b) [] = [(a,b)]
ordenaM (a,b) ((a1,b1):t) |b < b1 = (a,b):(a1,b1):t
                          |otherwise = (a1,b1):ordenaM (a,b) t

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)