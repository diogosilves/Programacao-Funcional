----------------
--1
----------------

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

--a)

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = -(calcula x)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

--b)

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "(-(" ++ infixa x ++ "))"
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

--c)

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = posfixa x ++ "(-)"
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ " + "
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ "-"
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ "*"


----------------
--2
----------------

data RTree a = R a [RTree a]
arvore = R 6 [R 4 [R 7 [R 1 [], R 3 []], R 9 []], R 3 [R 12 []], R 6 [], R 11 []]

--a)

soma :: Num a => RTree a -> a
soma (R r []) = r
soma (R r l) = r + sum (map soma l)

--b)

altura :: RTree a -> Int
altura (R r []) = 1
altura (R x l) = 1 + maximum (map altura l)

--c) 

prune ::  Int -> RTree a -> RTree a
prune 0 (R r _) = (R r [])
prune x (R r l) = R r $ map ( prune (x-1)) l

--d)
mirror :: RTree a -> RTree a
mirror (R r []) = R r []
mirror (R r l) = R r $ map mirror (reverse (l))

--e)
postorder :: RTree a -> [a]
postorder = undefined


----------------
--3
----------------

data LTree a = Tip a 
             | Fork (LTree a) (LTree a)

leafTree = Fork (Fork (Tip 5) (Fork (Tip 6) (Tip 4))) (Fork (Fork (Tip 3) (Tip 7)) (Tip 5))

--a)
ltSum :: Num a => LTree a -> a 
ltSum (Tip a) = a
ltSum (Fork x y) = (ltSum x) + (ltSum y)

--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork x y) = (listaLT x) ++ (listaLT y) 
 
--c)

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork x y) = 1 + max (ltHeight x) (ltHeight y)


----------------
--4
----------------

data BTree a = Empty 
             | Node a (BTree a) (BTree a)


--Full Tree
data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b)


--a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree = undefined

--b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees = undefined