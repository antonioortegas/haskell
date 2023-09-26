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



-- *##### EJERCICIO 6 ##### 



-- *##### EJERCICIO 7 ##### 



-- *##### EJERCICIO 8 ##### 



-- *##### EJERCICIO 9 ##### 



-- *##### EJERCICIO 10 ##### 



-- *##### EJERCICIO 11 ##### 



-- *##### EJERCICIO 12 ##### 



-- *##### EJERCICIO 13 ##### 



-- *##### EJERCICIO 14 ##### 



-- *##### EJERCICIO 15 ##### 



-- *##### EJERCICIO 16 ##### 



-- *##### EJERCICIO 17 ##### 


