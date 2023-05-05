--1
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo a b | b < a = []
                 |otherwise = a : enumFromTo (a+1) b

--2
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c | a > c = []
                       |otherwise = a : myEnumFromThenTo b (b-a) c

--3 (++)
concatenar :: [a] -> [a] -> [a]
concatenar [] l = l
concatenar l [] = l
concatenar (h:t) l = h : concatenar t l

--4 (!!)
finder :: [a] -> Int -> a
finder (h:_) 0 = h
finder (h:t) a = finder t (a-1)

--5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = (myReverse t) ++ [h]

--6
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake _ [] = []
mytake a (h:t) = h: mytake (a-1) t

--7
mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop _ [] = []
mydrop a (h:t) = mydrop (a-1) t

--8
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (h:t) (h1:t1) = (h,h1) : myzip t t1

--9
myreplicate :: Int -> a ->[a]
myreplicate 0 _ = []
myreplicate a b = b : myreplicate (a-1) b

--10
myintersperse :: a -> [a] -> [a] 
myintersperse a [] = []
myintersperse _ [a] = [a]
myintersperse a (h:t) = h : a : myintersperse a t

--11(REWATCH)
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [a] = [[a]]
mygroup(h:t)| elem h (head r) = (h : (head r)) : Prelude.tail r
              | otherwise = [h] : r
              where r = mygroup t


--12
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t

--13
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits(init l) ++ [l]

--14
mytails :: [a] -> [[a]]
mytails [] = []
mytails l = (l: mytails(Prelude.tail l))

--15
myheads :: [[a]] -> [a]
myheads [] = []
myheads (h:t) = (head h : myheads t)

--16
mytotal :: [[a]] -> Int
mytotal [] = 0
mytotal (h:t) = Prelude.length h + mytotal t

--17
myfun :: [(a,b,c)] -> [(a,c)]
myfun [] = []
myfun ((a,b,c):t) = ((a,c): myfun t)

--18
mycola :: [(String,b,c)] -> String
mycola [] = ""
mycola ((a,b,c):t) = a ++ mycola t

--19(TO DO)
--idade :: Int -> Int -> [(String,Int)] -> [String]

--20
myPowerEnumFrom :: Int -> Int -> [Int]
myPowerEnumFrom n 1 = [1]
myPowerEnumFrom n m | m > 1 = myPowerEnumFrom n (m-1) ++ [n^(m-1)]
                    |otherwise = []

--21
myIsPrime :: Int -> Bool
myIsPrime n | n >= 2 = primeCheck n 2 
            | otherwise = False

primeCheck :: Int -> Int-> Bool
primeCheck n m |m * m > n = True
               |mod n m == 0 = False 
               |otherwise = primeCheck n (m+1)


--22
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf l t | l == (take(Prelude.length l) t) = True
                 |otherwise = False

--23
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf [] _ = True
myIsSuffixOf _ [] = False
myIsSuffixOf l1 l2 = myIsPrefixOf(reverse l1)(reverse l2)

--24(TO DO)
--myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool

--25
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices a [] = []
myElemIndices a l = myIndices a l 0

myIndices :: Eq a => a -> [a] -> Int -> [Int]
myIndices _ [] _ = []
myIndices x (h:t) i | x == h = i : myIndices x t (i+1)
                    |otherwise = myIndices x t (i+1)

--26
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (h:t) |elem h t == False = h : myNub t
            |otherwise = myNub t

--27
myDelete :: Eq a => a -> [a] -> [a]
myDelete a [] = []
myDelete a (h:t) | a == h = t
                 |otherwise = h: myDelete a t

--28(\\)
doubleBar :: Eq a => [a] -> [a] -> [a]
doubleBar [] _ = []
doubleBar l [] = l
doubleBar (h1:t1) (h2:t2) | h1 == h2 = doubleBar t1 t2
                          | otherwise = h1 : doubleBar t1 (h2:t2)

--29
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] l = l
myUnion l [] = l
myUnion l (h:t) | elem h l == False = l ++ [h]  
                | otherwise = myUnion l t

--30
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect l [] = []
myIntersect [] l = []
myIntersect (h:t) l | elem h l == False = myIntersect t l
                    | otherwise = h : myIntersect t l

--31
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (h:t) | h > a = a:h:t
                 |otherwise = h: myInsert a t

--32
myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords (h:t) = h ++ (if null t then "" else " ") ++ myUnwords t

--33
myUnlines :: [String] -> String
myUnlines [] = "\n"
myUnlines (h:t) = h ++ "\n" ++ myUnlines t

--34(REVIEW)
myPMaior :: Ord a => [a] -> Int
myPMaior [] = 0
myPMaior (h:t) |h >= (t !! x) = 0
             |otherwise = 1 + x
    where x = myPMaior t 

--35
myLookUp :: Eq a => a -> [(a,b)] -> Maybe b 
myLookUp _ [] = Nothing
myLookUp n ((a,b):t) |n == a = Just b
                     |otherwise = myLookUp n t

