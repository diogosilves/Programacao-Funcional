nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c | (b^2 -4 * a * c) < 0 = 0
              | (b^2 -4 * a * c) == 0 = 1
              | otherwise = 2


----------------
--3
----------------

type Hora = (Int,Int)

--a)
valido :: Hora -> Bool
valido (h,t) | (h >= 0 && h < 24) && (t >= 0 && t < 60) = True
             | otherwise = False

--b)
maisTarde :: Hora -> Hora -> Bool
maisTarde (h,t) (h1,t1) |h < h1 = False
                        |h == h1 = (t > t1)
                        |h > h1 = True
                        |otherwise = False

--c)
conversorMin :: Hora -> Int
conversorMin (a,b) = 60*a + b

--d)
{-
conversorHora :: Int -> Hora
conversorHora a | a < 60 = (a,0)
                |otherwise = ()
-}

----------------
--5
----------------
data Semaforo = Verde | Amarelo | Vermelho deriving(Show,Eq)

--a)
next :: Semaforo -> Semaforo
next a | a == Verde = Amarelo
       | a == Amarelo = Vermelho
       | otherwise = Verde

--b) 
stop :: Semaforo -> Bool
stop a | a == Vermelho = True 
       | otherwise = False

--c)
safe :: Semaforo -> Semaforo -> Bool 
safe a b | (a == Verde) && (b == Vermelho) = True 
         | (b == Verde) && (a == Vermelho) = True
         | otherwise = False

----------------
--6
----------------
data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show,Eq)

--a)
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

--b)
posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar r a) = r * sin a

--c)
raio :: Ponto -> Double
raio (Polar r a) = r 
raio (Cartesiano x y) = sqrt(x^2 + y^2)

--d)
angulo :: Ponto -> Double
angulo (Polar r a) = a
angulo (Cartesiano x y) = atan(y/x)

--e)
dist :: Ponto -> Ponto -> Double
dist (Cartesiano x y) (Cartesiano x2 y2) = sqrt((x-x2)^2 + (y - y2)^2)
dist (Cartesiano x y) (Polar r a) = sqrt((x-x2)^2 + (y - y2)^2)
    where x2 = posx (Polar r a) 
          y2 = posy (Polar r a)
dist (Polar r a) (Cartesiano x y) = dist (Cartesiano x y) (Polar r a)
dist (Polar r a) (Polar r2 a2) = sqrt((x-x2)^2 + (y - y2)^2)
    where x = posx (Polar r a)
          y = posy (Polar r a)
          x2 = posx (Polar r a) 
          y2 = posy (Polar r a)


----------------
--7
----------------
data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

--a)
poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono (Retangulo _ _) = True
poligono (Triangulo _ _ _) = True

poligonoB :: Figura -> Bool
poligonoB (Circulo _ _) = False
poligonoB _ = False

vertices :: Figura -> [Ponto]
vertices (Triangulo a b c) = [a,b,c]
vertices (Retangulo (Cartesiano a b) (Cartesiano x y)) =
       [(Cartesiano a b)
       ,(Cartesiano x y)
       ,(Cartesiano a y)
       ,(Cartesiano x b)]
vertices (Circulo _ _) = []

dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x1 y1) (Cartesiano x2 y2) =
       sqrt((x1-x2)^2 + (y1-y2)^2)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
       let a = dist' p1 p2
           b = dist' p2 p3
           c = dist' p3 p1
           s = (a+b+c) / 2
       in sqrt (s*(s-a)*(s-b)*(s-c))

area(Circulo _ r) = pi * r^2

area (Retangulo (Cartesiano a b) (Cartesiano c d)) =
       let l1 = dist' (Cartesiano a b) (Cartesiano a d)
           l2 = dist' (Cartesiano a d) (Cartesiano c d)
       in l1 * l2



