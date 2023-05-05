
----------------
--1
----------------
data Hora = H Int Int
          deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--a)´

etapaValida :: Etapa -> Bool
etapaValida (a,b) |horaValida a && horaValida b = True
                  |otherwise = False
horaValida :: Hora -> Bool
horaValida (H h m) = h >= 0 && h < 24 && m >= 0 && m < 60

--b)
etapaEtapa :: Etapa -> Bool
etapaEtapa (a,b) = horaValida a && horaValida b && maiorHora a b

maiorHora :: Hora -> Hora -> Bool
maiorHora (H h1 m1) (H h2 m2) |h1 > h2 = True
                              |h1 == h2 && m1 > m2 = True
                              |otherwise = False

--c)
horaViagem :: Viagem -> Etapa
horaViagem v = (fst(head v), snd (last v))

--d)
tempoViagem :: [Etapa] -> Hora
tempoViagem [] = H 0 0
tempoViagem ((h1,h2):t) = undefined

somaHoras :: Hora -> Hora -> Hora
somaHoras (H h1 m1) (H h2 m2) = H (h1 + h2 + horasExtra) (mod (m1 + m2) 60)
    where horasExtra = div (m1 + m2) 60

subtracaoHoras :: Hora -> Hora -> Hora
subtracaoHoras (H h1 m1) (H h2 m2) = undefined

----------------
--4
----------------

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
           deriving Show
type TabDN = [(Nome,Data)]

--a)
amigos:: TabDN
amigos = [  ("Ana", D 1 1 2012)
         ,  ("To", D 2 2 2015)
         ,  ("Ze", D 10 6 1999)
         ]

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura n ((a,d):t) |n == a = Just d  
                    |otherwise = procura n t 

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade d n ((a,dn):ds) | n == a = Just (idadeNumaData dn d) 
                      |otherwise = idade d n ds

idadeNumaData :: Data -> Data -> Int
idadeNumaData (D _ _ an) (D _ _ ai) = ai - an