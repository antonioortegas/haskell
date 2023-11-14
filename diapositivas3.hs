-- TEMA 3 --

-- ! PARA TRABAJAR CON MODULOS
-- ! IMPORTAMOS EL MODULO CON:
import DataStructures.Stack.LinearStack as S
-- ! DONDE el nombre del archivo es la ruta del modulo
-- ! Las carpetas y modulos deben empezar por mayuscula
-- ! En el modulo correspondiente, se debe poner el nombre del modulo COMPLETO
-- ! Es decir, en el archivo LinearStack.hs, se debe poner:
-- ! module DataStructures.Stack.LinearStack
-- ! si tenemos varios modulos que importan algo con el mismo nombre, se puede poner:
-- ! import DataStructures.Stack.LinearStack as LS
-- ! y luego, para usarlo, se pone LS.empty, LS.push, etc.
import Test.QuickCheck
import DataStructures.Queue.LinearQueue as Q


-- *STACK

-- POLITICA LIFO (Last In First Out)
-- Operaciones básicas
-- push
-- pop
-- top
-- isEmpty

-- Las TAD vienen definidas por su interfaz, no por su implementación

-- isEmpty :: Stack a -> Bool
-- push :: a -> Stack a -> Stack a
-- top :: Stack a -> a
-- pop :: Stack a -> Stack a
-- empty :: Stack a

-- isEmpty y top son selectores, porque no modifican la estructura, sino que obtienen información de ella
-- pop es un modificador, porque modifica la estructura existente
-- push y empty son constructores, porque crean una estructura nueva

s1 :: Stack Int
s1 = push 3 (push 2 (push 1 S.empty))

sizes :: Stack a -> Int
sizes s | S.isEmpty s = 0
       | otherwise = 1 + sizes (pop s)

-- *QUEUE
-- POLITICA FIFO (First In First Out)
-- Operaciones básicas
-- enqueue añade un elemento al final de la cola
-- dequeue extrae el primer elemento de la cola
-- first devuelve el primer elemento de la cola
-- isEmpty comprar si la cola está vacía

q1 :: Queue Int
q1 = enqueue 1 (enqueue 2 (enqueue 3 Q.empty))

sizeq :: Queue a -> Int
sizeq q | Q.isEmpty q = 0
        | otherwise = 1 + sizeq (Q.dequeue q)

-- sizeq (enqueue 2 (enqueue 1 Q.empty)) == 2

-- *LISTAS
-- Las listas vienen predefinidas en Haskell

-- *SET
-- Un conjunto es una colección de elementos sin repetición
-- Operaciones básicas
-- isElem comprueba si un elemento pertenece al conjunto
-- insert añade un elemento al conjunto
-- delete elimina un elemento del conjunto
-- isEmpty comprueba si el conjunto está vacío