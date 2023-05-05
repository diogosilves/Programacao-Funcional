----------------
--1
----------------

--a)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (h:t) = f h || any f t

--b)

--myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--myZipWith f (h1:t1) (h2:t2) = f h1 h2 : myZipWith(f t1 t2)
--myZipWith _ _ _ = []

--c)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (h:t) | f h = h : myTakeWhile f t
                    | otherwise = myTakeWhile f t

--d)
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile f (h:t) | f h = dropWhile f t
                    | otherwise = h : dropWhile f t

--e)
mySpan :: (a-> Bool) -> [a] -> ([a],[a])
mySpan _ [] = ([],[])
mySpan f (h:t) | f h = let (taken, dropped) = span f t in (h:taken, dropped)             
           | otherwise = ([], h:t)

--f)
myDeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
myDeleteBy _ _ [] = []
myDeleteBy f x (h:t) | f x h = t 
                     | otherwise = h : myDeleteBy f x t

--g)
mySortOn ::  Ord b => (a -> b) -> [a] -> [a]
mySortOn _ [] = []
mySortOn f (h:t) = undefined


----------------
--2
----------------

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
mySelgrau :: Int -> Polinomio -> Polinomio
mySelgrau _ [] = []
mySelgrau n ((x,y):t) | n == y = (x,y) : mySelgrau n t
                      | otherwise = mySelgrau n t

--b)
myConta :: Int -> Polinomio -> Int
myConta _ [] = 0
myConta n ((x,y):t) | n == y = 1 + myConta n t
                    | otherwise = myConta n t

--c)
myGrau :: Polinomio -> Int
myGrau [] = 0
myGrau p = maximum $ map snd p

--d)
myDeriv :: Polinomio -> Polinomio 
myDeriv [] = []
myDeriv ((x,y):t) = (x* fromIntegral y,y-1) : myDeriv t

--e)
myCalcula :: Float -> Polinomio -> Float
myCalcula n [] = 0
myCalcula n ((x,y):t) = ((x*n)^y) + myCalcula n t

--f)
mySimp :: Polinomio -> Polinomio
mySimp [] = []
mySimp ((x,y):t) | y == 0 = mySimp t
                 | otherwise = (x,y): mySimp t

--g)
myMult :: Monomio -> Polinomio -> Polinomio
myMult (cm,gm) = map (\(c,g) -> (cm*c,gm+g))

--h)
myOrdena :: Polinomio -> Polinomio
myOrdena [] = []
myOrdena ((x,y):t) = undefined

----------------
--3
----------------

type Mat a = [[a]]

--a)
dimOK :: Mat a -> Bool
dimOK m = let cl = length(head m)
          in and (map(\l -> length l == cl) m)

--b)
dimMat :: Mat a -> (Int, Int)
dimMat m = (length m, length (head m))


--c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))


--d)
transpose :: Mat a -> Mat a
transpose m = [ map (!! i) m |  i <- [0..c-1]]
    where (l,c) = dimMat m

--e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = map (\l -> [ sum (zipWith (*) l [ x !! i | x <- m2 ]) | i <- [0..c-1] ]) m1
    where (l,_) = dimMat m1
          (_,c) = dimMat m2

--f)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith

addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' = zipWMat (+)

--g)
triSup :: Real a => Mat a -> Bool
triSup m = and [ all (== 0) [ m !! i !! j | j <- [0..i-1] ] | i <- [0..length m - 1]]

--h)
rotateLeft :: Mat a -> Mat a
rotateLeft m = [ map (!! i) m | i <- [c-1,c-2..0]] 
    where (l,c) = dimMat m