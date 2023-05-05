----------------
--1
----------------

data Frac = F Integer Integer

--a)
normaliza :: Frac -> Frac 
normaliza (F a b) 
    | b < 0 = normaliza $ F (-a) (-b)
    | otherwise =
        let d = mdc a b in
            F (a `div` b) (b `div` d)

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc 0 y = y
mdc x y = mdc y (x `div` y)

--b)
instance Eq Frac where
    (F a b) == (F c d) = a * d == c * b

--c)
instance Ord Frac where
    (F a b) <= (F c d) = a * d <= c * b

--d)
instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

--e)
class (Eq a, Show a) => Num a where
    (+), (*), (-) :: a -> a -> a
    negate, abs, signum :: a -> a
    fromInteger :: Integer -> a

instance Num Frac where
    (+) :: Frac -> Frac -> Frac
    (F a b) + (F c d) | b == d = normaliza $ F (a + c) b
                      | otherwise = normaliza $ F (a * d + b * c) (b * d)
    
    (-) :: Frac -> Frac -> Frac
    x - y = x + negate y

    (*) :: Frac -> Frac -> Frac
    (F a b) * (F c d) = F (a * c) (b * d)
    
    negate :: Frac -> Frac
    negate (F a b) = F (-a) b
    
    abs :: Frac -> Frac
    abs (F a b) = F (abs a) (abs b)
    
    signum :: Frac -> Frac
    signum (F a b) | a == 0 = F 0 1
                   | a * b > 0 = F 1 1
                   | otherwise = F (-1) 1
    
    fromInteger :: Integer -> Frac
    fromInteger x = F x 1

--f)
maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro = filter . (<) . (2 *)

----------------
--2
----------------

data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

--a)
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

--b)
valueOf :: Num a => Exp a -> a
valueOf (Const a) = a
valueOf (Simetrico a) = negate $ valueOf a
valueOf (Mais a b) = valueOf a + valueOf b
valueOf (Menos a b) = valueOf a - valueOf b
valueOf (Mult a b) = valueOf a * valueOf b

instance (Num a, Eq a) => Eq (Exp a) where
    (==) :: (Num a, Eq a) => Exp a -> Exp a -> Bool
    a == b = valueOf a == valueOf b

--c)

instance (Ord a, Num a) => Prelude.Num (Exp a) where
    (+) :: Num a => Exp a -> Exp a -> Exp a
    x + y = Mais x y
    
    (-) :: Num a => Exp a -> Exp a -> Exp a
    x - y = Menos x y
    
    (*) :: Num a => Exp a -> Exp a -> Exp a
    x * y = Mult x y
    
    negate :: Num a => Exp a -> Exp a
    negate (Simetrico a) = a
    negate a = Simetrico a
    
    fromInteger :: Num a => Integer -> Exp a
    fromInteger x = Const (fromInteger x)
    
    abs :: (Ord a, Num a) => Exp a -> Exp a
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs op = if valueOf op < 0 
             then negate op
             else op
    
    signum :: Num a => Exp a -> Exp a
    signum a | valueOf a < 0 = Const (-1)
             | valueOf a == 0 = Const 0
             | otherwise = Const 1 