import Data.Char
import Test.QuickCheck

--identificadores empiezan por minuscula o "_", y los siguientes pueden ser letras, digitos, tildes o _
-- f    f'  f2  _uno    dos     toDo        son validos
-- F    F2  f!y                             no lo son

--los tipos empiezan por mayuscula
-- Int, Integer, Bool, Float, Double...

--los operadores tienen uno o varios simbolos, pero el primero no puede ser ":"
-- ejemplos son    +       ++      ==>      !!

-- Int es un subconjunto acotado de Integer para operaciones rapidas y simples (hay overflow)
-- Integer es el conjunto completo

--Float es un subconjunto de los reales en precision simple. Double es para precision doble

--Bool solo puede ser True / False

--estan las conjunciones logicas && , ||, ... LA EVALUACION ES EN CORTOCIRCUITO

--Char es el conjunto de caracteres unicode
--hay muchas funciones predefinidas en Data.Char (toUpper por ejemplo)

--se escribe
-- identificador :: tipodeentrada -> tipodeentrada2 -> ... -> tipodesalida
-- identificador parametro1 parametro2 ... = operaciones a realizar

twice :: Integer -> Integer
twice x = x + x

square :: Integer -> Integer
square x = x * x 

sumOfSquares :: Integer -> Integer -> Integer
sumOfSquares x y = square x + square y

-- para hacer el cuadrado de 5+1, se deben poner parentesis, de lo contrario hara (square de 5) +1
-- square (5 + 1)
--ANTE LA DUDA, PARENTESIS AL CANTO

--para condicionales
maxInteger :: Integer -> Integer -> Integer
maxInteger x y = if x >= y then x else y 

--recursividad
factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
-- se usa Integer porque Int hace overflow muy rapidamente con el factorial

--el operador "-" es el unico operador simbolico PREFIJO (-2)
-- 1 + -2 da error, pero 1 + (-2) no
--el resto se usa de forma infija (entre medias) como 1 + 2
--como rareza, se pueden usar de forma infija las funciones y sus dos argumentos, por ejemplo
-- 2 `max` 5 es valido, y da 5
--tb los operadores se pueden usar de forma prefija con parentesis
-- (+) 2 5 da como resultado 7

--hay niveles de PRECEDENCIA, pero como no pienso memorizarmelos, si no los puedo mirar va parentesis a todo

--igual con la ASOCIATIVIDAD, parentesis ante la duda (por ejemplo no se puede poner x == y == z)
-- no es lo mismo que la propiedad matematica de una operacion de ser asociativa

--TUPLAS
--coleccion de valores (1, dos, False) o (4, 5) son tuplas
--sus tipos son (Int, Char, Bool) y (Int, Int)
succPred :: Int -> (Int, Int)
succPred x = (x+1, x-1)

fst2 :: (a,b) -> a
fst2 (x,y) = x

--SOBRECARGA
--(+), (-), (*) :: (Num a) => a -> a -> a
--Num es un tipo sobrecargado (a debe ser cualquier tipo numerico)

twice2 :: Num a => a -> a
twice2 x = x + x

--Estan Integral para enteros, Num para numericos, Fractional para numeros racionales y flotantes, Eq para los tipo en los que tiene sentido la igualdad...

--Ord es la clase de los tipos para los cuales existe una relación de orden total
--hay operaciones como max , min o ">=" que solo tienen sentido en estos tipos

--GUARDAS (las condicionales son una alternativa a las guardas)
--A menudo es más sencillo describir una función por partes (distintos casos)
sign :: (Eq a, Ord a, Num a) => a -> a
sign x  | x > 0 = 1
        | x < 0 = -1
        | x == 0 = 0

--Uso de OTHERWISE (guarda que siempre es TRUE)
sign2 :: (Ord a, Num a) => a -> a
sign2 x  | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0

--Funciones definidas PARCIALMENTE
--reciprocal 0 da error
reciprocal :: (Eq a, Fractional a) => a -> a
reciprocal x    | x == 0 = error "reciprocal is undefined"
                | otherwise = 1 / x

--definiciones locales con WHERE
circArea :: Double -> Double
circArea r = pi*r^2

rectArea :: Double -> Double -> Double
rectArea b h = b*h

circLength :: Double -> Double

circLength r = 2*pi*r
cylinderArea :: Double -> Double -> Double

cylinderArea r h = 2*circ + rect
    where
        circ = circArea r
        l = circLength r
        rect = rectArea l h
--OTRA OPCION CON LET IN SERIA
cylinderArea2 :: Double -> Double -> Double
cylinderArea2 r h =
    let
        circ = circArea r
        l = circLength r
        rect = rectArea l h
    in
        2*circ + rect

--declaracion de OPERADOES

infix 4 ~= --infix es no asociativo. infixl lo es a la izq y infixr a la derecha. 4 indica nivel de precedencia
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000 --comprueba si dos reales no distan mas de una milesima



--QUICKCHECK
--por ejemplo escribimos propiedades para la funcione square
square2 :: (Num a) => a -> a
square2 x = x * x

p1 x y = True ==> square2 (x+y) == square2 x + square2 y + 2*x*y    --se cumple
p2 x y = True ==> abs (x+y) == abs x + abs y                        -- debe fallar
p3 x y = x>=0 && y>=0 ==> abs (x+y) == abs x + abs y                --se cumple

--check property with
--quickCheck (p1 :: Integer -> Integer -> Property)