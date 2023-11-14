-- TEMA 2 --

-- *LISTAS
-- Tienen que ser homogeneas, es decir, todos los elementos de la lista deben ser del mismo tipo

-- ? [1,2,3] :: [Int]
-- ? [1.0,2.0,3.0] :: [Double]
-- ? [('a', True),('b', False)] :: [(Char, Bool)]

-- Operaciones basicas incluidas en el prelude
-- ? head :: [a] -> a (devuelve la cabeza de la lista, el primer elemento)
-- ? tail :: [a] -> [a] (devuelve la cola de la lista, todos los elementos menos el primero)
-- ? null :: [a] -> Bool (True si la lista es vacia)

-- ? (:) :: a -> [a] -> [a] (operador de construccion de listas, se lee como "cons")
-- ? 1 : [2,3] == [1,2,3]
-- ? 1 : 2 : 3 : [] == [1,2,3]

-- En Haskell, un String es una lista de caracteres
-- ? type String = [Char]
-- por ello, se pueden escribir strings como "hola" o ['h','o','l','a']

-- Para definir funciones sobre listas, existen patrones de definicion
-- ? [] es la lista vacia
-- ? [x] es una lista con un elemento
-- ? [x,y] es una lista con dos elementos
-- ? (x:xs) es una lista con al menos un elemento, donde x es la cabeza y xs la cola
-- ? (x:y:zs) es una lista con al menos dos elementos, donde x e y son la cabeza y ys la cola (lista sin x ni y)

-- ? length :: [a] -> Int (devuelve la longitud de la lista)
-- ES RECURSIVA, se puede definir como
-- length :: [a] -> Int
-- length [] = 0
-- length (x:xs) = 1 + length xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True -- lista con un elemento, sin importar cual
sorted (x:y:zs) = x <= y && sorted (y:zs)

-- ? infixr 5 ++ (operador de concatenacion de listas)
-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

-- ? [1,2] ++ [3,4] == [1,2,3,4]

-- ? reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]
-- OTRA OPCION
-- reverse xs = revOn xs []
--  where
--   revOn [] ys = ys
--   revOn (x:xs) ys = revOn xs (x:ys)

-- Esta segunda opcion utilizada los llamados "acumuladores", que son variables que se van pasando de una llamada a otra
-- Se parecen a los BUCLES de los lenguajes imperativos

factorialAc :: Integer -> Integer
factorialAc n = factOn n 1
 where
  factOn 0 acc = acc
  factOn n acc = factOn (n-1) (n*acc)

-- SUBLISTAS

-- ? take :: Int -> [a] -> [a] (devuelve los primeros n elementos de la lista)
-- ? drop :: Int -> [a] -> [a] (devuelve la lista sin los n primeros elementos)

-- *FUNCIONES DE ORDEN SUPERIOR

-- Son funciones que toman funciones como argumentos y/o devuelven funciones como resultado
-- Se pueden usar para diferenctes propositos, como por ejemplo, aplicar una funcion cualquiera pasada como argumento
-- a todos los elementos de una lista

-- ? map :: (a -> b) -> [a] -> [b]      ( a -> b ) es una funcion que toma un a y devuelve un b
-- map f [] = []
-- map f (x:xs) = f x : map f xs

-- map square [1,2,3] == [1,4,9]

-- ? filter :: (a -> Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p (x:xs) 
--  | p x = x : filter p xs
--  | otherwise = filter p xs

-- filter even [1,2,3,4] == [2,4]
-- filter (== 'a') "abracadabra" == "aaaaa"
-- filter isDigit "a1b2c3" == "123"

-- *FUNCIONES LAMBDA
-- Podemos pasar funciones como argumentos a otras funciones, pero a veces es molesto tener que definir una funcion
-- solo para pasarla como argumento a otra funcion
-- Para ello, se pueden definir funciones "anonimas" usando la palabra clave "lambda"

-- ? (\x -> x + 1) 5 == 6        (funcion que toma un argumento y le suma 1)
-- ? (\x y -> x + y) 5 6 == 11   (funcion que toma dos argumentos y los suma)
-- notese que la funcion lambda por ejemplo en este segundo caso podemos interpretarla como una funcion que toma dos argumentos y los suma
-- ? filter (\x -> x > 5) [1..10] == [6,7,8,9,10]
-- aqui filter necesita una funcion como primer argumento que devuelva Bool, asi que definimos una funcion lamba que dado un valor x, nos dice si es mayor que 5

-- SECCIONES
-- Podemos aplicar un unico argumento a un operacdor binario para obtener una funcion del argumento suprimido
-- ? (+1) == (\x -> x + 1)
-- ? map (+1) [1,2,3] == [2,3,4]
-- ? map (2*) [1,2,3] == [2,4,6]
-- ? filter (>5) [1..10] == [6,7,8,9,10]

-- SECUENCIAS
-- Las secuencias son listas cuyos elementos son numeros enteros consecutivos
-- ? [1..10] == [1,2,3,4,5,6,7,8,9,10]
-- ? [1,3..10] == [1,3,5,7,9]
-- ? [1..] == [1,2,3,4,5,6,7,8,9,10,...] (lista infinita de numeros naturales)

-- LISTAS POR COMPRENSION (en lugar de por extension)
-- Para generar listas de esta forma:
-- ? [ expresion | patron <- lista ] 
-- Es decir, se genera una lista aplicando la expresion a cada elemento de la lista que cumpla el patron

-- ? [ x + 1 | x <- [1,2,3] ] == [2,3,4]
-- ? [ even x | x <- [1..10] ] == [False,True,False,True,False,True,False,True,False,True]
-- ? [ (x, even x) | x <- [1..4] ] == [(1,False),(2,True),(3,False),(4,True)]

-- guardas
-- ? [ x | x <- [1..10], even x ] == [2,4,6,8,10]
-- ? [ x | x <- [1..10], even x, x > 5 ] == [6,8,10]
-- ? [ x^2 | x <- [1..10], even x] == [4,16,36,64,100]     (solo los cuadrados de los pares hasta 10, combino map y filter)

--puedo hacer tambien definiciones locales
-- ? [ (x, y) | x <- [1..3], let y = x^2 ]  == [(1,1),(2,4),(3,9)]
-- es equivalente a
-- ? [ (x, 2*x) | x <- [1..3] ]             == [(1,2),(2,4),(3,6)]

-- generadores multiples
-- ? [ (x, y) | x <- [1..3], y <- [4..6] ]  == [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- esto es el producto cartesiano de las dos listas

-- PLEGADO DE LISTAS
-- ? foldr :: (a -> b -> b) -> b -> [a] -> b
-- ? foldr f z [] = z
-- ? foldr f z (x:xs) = f x (foldr f z xs)
-- donde (a -> b -> b) es una funcion binaria que opera dos elementos
-- b es el valor inicial
-- [a] es la lista a plegar
-- b es el valor final

-- ? foldr add 0 [1,2,3] == 6
-- donde add era
-- ? add :: Int -> Int -> Int
-- ? add x y = x + y
-- suma 0 y 3, el resultado lo suma con 2, el resultado lo suma con 1. El resultado es 6

-- ? foldl lo mismo pero con la lista plegada de izquierda a derecha
-- ? foldl add 0 [1,2,3] == 6
-- suma 0 y 1, el resultado lo suma con 2, el resultado lo suma con 3. El resultado es 6

-- en prelude, muchas funciones sobre listas estan definidas con plegados
-- concat, sum, or, and, product son ejemplos

-- Plegado de listas y expresiones lamba con multiples argumentos
-- ? foldl (\x y -> x^2 - y) 1 [1,2..5]         == 4
-- ! foldr (\x y -> x^2 - y) 1 [1,2..5]         == 14


-- *PARCIALIZACION

-- Podemos expresar una funcion de varios argumentos como una cadena de funciones de un solo argumento
f1 :: Int -> Int -> Int -> Int
f1 x y z = x + 2*y + 3*z
-- equivale a
f2 :: Int -> (Int -> (Int -> Int))
f2 = \x -> (\y -> (\z -> x + 2*y + 3*z))
-- equivale a
f3 :: Int -> (Int -> (Int -> Int))
f3 = \x -> \y -> \z -> x + 2*y + 3*z
-- f3 10 20 30 == 10 + 2*20 + 3*30 == 140

-- *REGLA FUNDAMENTAL DE LA TIPIFICACION
-- SI
-- ? f :: a -> b
-- ? x :: a
-- ENTONCES
-- ? f x :: b

-- ejemplo1: 
-- even :: Int -> Bool
-- 10 :: Int
-- entonces
-- even 10 :: Bool

-- ejemplo2:
-- take :: Int -> [a] -> [a]
-- 10 :: Int
-- [True, False] :: [Bool]
-- entonces
-- take 10 :: [a] -> [a]
-- take 10 [True, False] :: [Bool]

-- *COMPOSICION DE FUNCIONES
-- el operador (.) permite componer funciones
-- ? (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- ? f . g = \x -> f (g x)

-- es decir
isOdd :: Int -> Bool
isOdd = not . even
-- entonces
-- isOdd 10 == not (even 10) == not False == True
-- isOdd 10 primero aplica even 10, que es False, y luego aplica not a False, que es True

-- *TIPOS DE DATOS ALGEBRAICOS
-- Son tipos de datos definidos por el usuario, que se pueden definir con
-- ? data NombreTipo = Constructor1 Tipo1 Tipo2 ... | Constructor2 Tipo3 Tipo4 ... | ...
data Direction = North | South | East | West
-- es un tipo enumerado (conjunto finito de valores)

-- *CLASES DE TIPOS
-- Son conjuntos de tipos que tienen ciertas operaciones en comun
-- Por ejemplo, la clase de tipos Eq es una clase de tipos cuyos elementos tienen una operacion de igualdad y una operacion de desigualdad

-- Podemos definir una instancia de una clase de tipos para un tipo de datos definido por el usuario
instance Eq Direction where
    North == North = True
    South == South = True
    East == East = True
    West == West = True
    _ == _ = False
-- es decir, definimos la igualdad para el tipo Direction, donde dos direcciones son iguales si son la misma
-- y son distintas en caso contrario

-- Ahora podemos hacer cosas como
-- ? North == South             == False
-- ? North /= South             == True
-- ? West == West               == True

-- Otro ejemplo:
data Ordering = LT | EQ | GT
-- (viene definido en prelude)

instance Ord Direction where
    -- Para North < South < East < West
    North <= _ = True
    South <= North = False
    South <= _ = True
    East <= North = False
    East <= South = False
    East <= _ = True
    West <= _ = False

-- Ahora podemos hacer 
-- ? North < South              == True
-- ? South < North              == False 

-- *LA CLASE SHOW
-- DEFINE COMO SE MUESTRAN LOS DATOS
-- Predefinida como
-- ? class Show a where
-- ?   show :: a -> String

instance Show Direction where
    show North = "North"
    show South = "South"
    show East = "East"
    show West = "West"

-- Entonces
-- ? show North             == "North"

-- *GENERACION AUTOMATICA DE INSTANCIAS
-- Podemos generar automaticamente instancias de clases de tipos para tipos de datos definidos por el usuario
-- ? data Direction = North | South | East | West deriving (Eq, Ord, Show)
-- Esto considera valores identicos como iguales para Eq
-- Considera valores en el orden en el que estan definidos para Ord

-- *TIPOS UNION
--VARIOS CONSTRUCTORES DE DATOS

data Degrees = Celsius Double | Fahrenheit Double deriving Show

-- los constructores se pueden usar en patrones
frozen :: Degrees -> Bool
frozen (Celsius x) = x <= 0
frozen (Fahrenheit x) = x <= 32

toCelsius :: Degrees -> Degrees
toCelsius (Fahrenheit x) = Celsius (5/9 * (x - 32))
toCelsius x = x

toFahrenheit :: Degrees -> Degrees
toFahrenheit (Celsius x) = Fahrenheit (9/5 * x + 32)
toFahrenheit x = x

-- frozen (Celsius 20)              == False
-- toCelcius (Fahrenheit 32)        == Celsius 0
-- toFahrenheit (Celsius 0)         == Fahrenheit 32

-- Predefinido en prelude
-- data Either a b = Left a | Right b
-- data Maybe a = Nothing | Just a
-- lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- lookup x [] = Nothing
-- lookup x ((y,z):yzs)
--     | x == y = Just z
--     | otherwise = lookup x yzs

-- entonces
-- ? lookup 2 [(1, "one"), (2, "two"), (3, "three")] == Just "two"
-- ? lookup 4 [(1, "one"), (2, "two"), (3, "three")] == Nothing

-- *TIPOS PRODUCTO
-- UN UNICO CONSTRUCTOR, VARIOS COMPONENTES

type Name = String
type Surname = String
type Age = Int

data Person = Person Name Surname Age deriving Show
john :: Person
john = Person "John" "Smith" 30

name :: Person -> Name
name (Person n _ _) = n

surname :: Person -> Surname
surname (Person _ s _) = s

age :: Person -> Age
age (Person _ _ a) = a

-- name john == "John"
-- surname john == "Smith"
-- age john == 30
-- show john == "Person \"John\" \"Smith\" 30"

-- *ORDENAR UNA LISTA DE FORMA RECURSIVA
-- ? sort :: Ord a => [a] -> [a]
-- ? sort [] = []
-- ? sort (x:xs) = sort smaller ++ [x] ++ sort larger
-- ?     where
-- ?         smaller = [ y | y <- xs, y <= x ]
-- ?         larger = [ y | y <- xs, y > x ]

-- sort [3,2,1,4,5] == [1,2,3,4,5]
-- sort "haskell" == "aehklls"