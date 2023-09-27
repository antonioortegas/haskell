-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: Ortega Santaolalla, Antonio
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck

-- *##### EJERCICIO 1 ##### 

esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x * x + y * y == z * z

terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = if x > y then (x * x - y * y, 2 * x * y, x * x + y * y) else error "x debe ser mayor que y"

p_ternas x y = x > 0 && y > 0 && x > y ==> esTerna l1 l2 h
  where
    (l1, l2, h) = terna x y

-- quickCheck (p_ternas :: Integer -> Integer -> Property)

-- *##### EJERCICIO 2 ##### 

intercambia :: (a, b) -> (b, a)
intercambia (a, b) = (b, a)

-- *##### EJERCICIO 3 ##### 

ordena2 :: Ord a => (a, a) -> (a, a)
ordena2 (a, b) = if a < b then (a, b) else (b, a)

p1_ordena2 x y = enOrden (ordena2 (x, y)) -- comprueba que en la tupla resultante el elemento 1 es menor que el elemento 2
  where
    enOrden (x, y) = x <= y

-- quickCheck (p1_ordena2 :: Ord a => (a,a) -> (a,a) -> Bool)

p2_ordena2 x y = mismosElementos (x, y) (ordena2 (x, y)) -- la tupla resultante contiene los dos mismos elementos que la entrada, se hayan ordenado o no
  where
    mismosElementos (x, y) (z, v) = (x == z && y == v) || (x == v && y == z)

-- quickCheck (p2_ordena2 :: Ord a => (a,a) -> (a,a) -> Bool)

ordena3 :: Ord a => (a, a, a) -> (a, a, a)
ordena3 (x, y, z)
  | z < y = ordena3 (x, z, y)
  | y < x = ordena3 (y, x, z)
  | otherwise = (x, y, z)

p1_ordena3 (x, y, z) = True ==> enOrden (ordena3 (x, y, z)) -- comprueba que en la tupla resultante los elementos estan ordenados
  where
    enOrden (x, y, z) = x <= y && y <= z
-- quickCheck (p1_ordena3 :: Ord a => (a,a,a) -> Property)

p2_ordena3 (x, y, z) = mismosElementos (x, y, z) (ordena3 (x, y, z)) -- la tupla resultante contiene los tres mismos elementos que la entrada
  where
    mismosElementos (x, y, z) (a, b, c) = (x == a && y == b && z == c) || (x == a && y == c && z == b) || (x == b && y == a && z == c) || (x == b && y == c && z == a) || (x == c && y == a && z == b) || (x == c && y == b && z == a)
-- quickCheck (p2_ordena3 :: Ord a => (a,a,a) -> Bool)

-- *##### EJERCICIO 4 ##### 

max2 :: Ord a => a -> a -> a
max2 x y = if x > y then x else y

p1_max2 (a, b) = True ==> (max2 a b == a) || (max2 a b == b)
p2_max2 (a, b) = True ==> (max2 a b >= a) && (max2 a b >= b)
p3_max2 (a, b) = a >= b ==> max2 a b == a
p4_max2 (a, b) = b >= a ==> max2 a b == b

-- quickCheck (p1_max2 :: Ord a => (a,a) -> Property)
-- quickCheck (p2_max2 :: Ord a => (a,a) -> Property)
-- quickCheck (p3_max2 :: Ord a => (a,a) -> Property)
-- quickCheck (p4_max2 :: Ord a => (a,a) -> Property)

-- *##### EJERCICIO 5 ##### 

entre :: Ord a => a -> (a,a) -> Bool
entre x (y, z)
  | y <= x && x <= z = True
  |otherwise = False

-- *##### EJERCICIO 6 ##### 

iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) = x == y && x == z

-- *##### EJERCICIO 7 ##### 

type TotalSegundos  = Integer
type Horas          = Integer
type Minutos        = Integer
type Segundos       = Integer
descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas, minutos, segundos)
  where
    horas = div x 3600
    minutos = div (mod x 3600) 60
    segundos = mod x 60

p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x
                            && entre m (0,59)
                            && entre s (0,59)
  where (h,m,s) = descomponer x

-- *##### EJERCICIO 8 ##### 

unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x*unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) == x
-- !DA ERROR POR ERROR DE REDONDEO DE PUNTO FLOTANTE

-- *##### EJERCICIO 9 ##### 

infix 4 ~=

(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
  where epsilon = 1/1000

p_inversas2 x = eurosAPesetas (pesetasAEuros x) ~= x

-- TODO##### EJERCICIO 10 ##### 

raices :: Double -> Double -> Double -> (Double, Double)
raices x y z
  | root < 0 = error "test"
  | 2*x == 0 = error "det es 0"
  | otherwise = (r1, r2)
    where
      r1 = (-y + sqrt root) / 2*x
      r2 = (-y - sqrt root) / 2*x
      root = y*y - (4*x*z)

p1_raices a b c = esRaiz r1 && esRaiz r2
 where
  (r1,r2) = raices a b c
  esRaiz r = a*r*r + b*r + c ~= 0

p1_raices2 a b c = (b*b - (4*a*c)) >= 0 && a /= 0 ==> esRaiz r1 && esRaiz r2
 where
  (r1,r2) = raices a b c
  esRaiz r = a*r*r + b*r + c ~= 0


-- *##### EJERCICIO 11 ##### 



-- *##### EJERCICIO 12 ##### 



-- *##### EJERCICIO 13 ##### 



-- *##### EJERCICIO 14 ##### 

potencia :: Integer -> Integer -> Integer
potencia b n
  | n == 1 = b
  | otherwise = b * potencia b (n-1)

potencia' :: Integer -> Integer -> Integer
potencia' b n
  | n == 0 = 1
  | n == 2 = b*b
  | mod n 2 == 0 = inside * inside
  | otherwise = b * inside2 * inside2
    where inside = potencia' b (div n 2)
          inside2 = potencia' b (div (n-1) 2)

-- *##### EJERCICIO 15 ##### 



-- *##### EJERCICIO 16 ##### 



-- *##### EJERCICIO 17 ##### 


