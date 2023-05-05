data BTree a = Empty
            | Node a (BTree a) (BTree a)
        deriving Show


----------------
--1
----------------

--a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = 1 + max (altura e) (altura d)

--b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

--c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d

--d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node r e d ) = Node r (prune (x-1) e) (prune(x-1) d)

--e)                   NOT SURE

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node r _ _) = [r]
path (h:t) (Node r e d) | h = r : path t e
                        |otherwise = r : path t d

--f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

--g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--h)                (TODO)

unzip :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzip Empty = (Empty, Empty, Empty)
unzip _ = undefined


----------------
--2
----------------

--a)

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r e d) = minimo e

--b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node _ Empty Empty) = Empty
semMinimo (Node r e d) = semMinimo e 

--c)            FUNCTION??
minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin _ = undefined

--d)            FUNCTION??
remove :: Ord a => a -> BTree a -> BTree a
remove _ _ = undefined


----------------
--3
----------------

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno --árvore binária de procura (ordenada por número)

--a)
inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False
inscNum x (Node (n,_,_,_) e d) | x == n = True 
                               | otherwise = inscNum x (if x < n then e else d)

--b)
inscNome :: Nome -> Turma -> Bool
inscNome x Empty = False
inscNome x (Node (_,n,_,_) e d) | x == n = True 
                                | otherwise = (inscNome x e || inscNome x d)

--c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,TE,_) e d) = trabEst e ++ [(num,nome)] ++ trabEst d
trabEst (Node _ e d) = trabEst e ++ trabEst d

--d)
nota :: Numero -> Turma -> Maybe Classificacao
nota x (Node (n,_,_,c) e d)
    |x == n = Just c
    |x < n = nota x e
    |otherwise = nota x d
nota _ _ = Nothing

--e)
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = nFaltas turma / nAlunos turma * 100
    where nFaltas :: Turma -> Float
          nFaltas Empty = 0
          nFaltas (Node (_,_,_,Faltou) e d) = (1 + nFaltas e + nFaltas d)
          sumFaltas (Node _ e d) = sumFaltas e + sumFaltas d
          nAlunos = fromIntegral.contaNodos

--f)
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov x = undefined 

--g)
aprovAv :: Turma -> Float
aprovAv = undefined