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
-- ? DA ERROR POR ERROR DE REDONDEO DE PUNTO FLOTANTE

-- *##### EJERCICIO 9 ##### 

infix 4 ~=

(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
  where epsilon = 1/100

p_inversas2 x = eurosAPesetas (pesetasAEuros x) ~= x

-- TODO ##### EJERCICIO 10 ##### 

raices :: Double -> Double -> Double -> (Double, Double)
raices a b c
  | a == 0 = error "no es de grado 2"
  | determinante < 0 = error "el determinante no puede ser negativo"
  | otherwise = ((-b + sqrt determinante) / 2 * a, (-b - sqrt determinante) / 2 * a)
    where
      determinante = (b*b) - (4*a*c)

p1_raices a b c = esRaiz r1 && esRaiz r2
 where
  (r1,r2) = raices a b c
  esRaiz r = a*r*r + b*r + c ~= 0

p1_raices2 a b c = determinante >= 0 && a /= 0 ==> esRaiz r1 && esRaiz r2
 where
  (r1,r2) = raices a b c
  esRaiz r = a*r^2 + b*r + c ~= 0
  determinante = (b*b) - (4*a*c)


-- *##### EJERCICIO 11 ##### 

esMultiplo :: Integral a => a -> a -> Bool
esMultiplo x y = mod x y == 0 

-- *##### EJERCICIO 12 ##### 

infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
a ==>> b
  | a == True && b == False = False
  | otherwise = True

-- *##### EJERCICIO 13 ##### 

esBisiesto :: Integer -> Bool
esBisiesto x = (mod x 4 == 0) && (mod x 100 == 0 ==>> mod x 400 == 0) 

-- TODO ##### EJERCICIO 14 ##### 

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
          inside2 = potencia' b (div (n -1) 2)

p_pot b n = n>=0 ==> potencia b n == sol && potencia' b n == sol
  where sol = b^n

-- *##### EJERCICIO 15 ##### 

factorial :: Integer -> Integer
factorial x
  | x == 1 = x
  | otherwise = x * factorial (x-1)

-- *##### EJERCICIO 16 ##### 

divideA :: Integer -> Integer -> Bool
divideA x y = mod y x == 0

p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x
-- ? dos condiciones : (a) el divisor no es cero (b) siempre que y divida a x, (x/y) * y == x

p2_divideA x y z = x /= 0 && (x `divideA` y && x `divideA` z) ==>> (x `divideA` (y+z))

-- *##### EJERCICIO 17 ##### 

mediana :: Ord a => (a,a,a,a,a) -> a
mediana (a, b, c, d, e)
  | a > b = mediana (b, a, c, d, e)
  | b > c = mediana (a, c, b, d, e)
  | c > d = mediana (a, b, d, c, e)
  | d > e = mediana (a, b, c, e, d)
  | otherwise = c