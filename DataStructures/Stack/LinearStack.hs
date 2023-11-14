-------------------------------------------------------------------------------
-- Linear implementation of Sets with nodes sorted according to values
-- and non-repeated elements
--
-- Data Structures. Grado en Informática. UMA.
--
-- STUDENT'S NAME:
-------------------------------------------------------------------------------

module DataStructures.Stack.LinearStack (
    Stack
    , empty
    , isEmpty
    , push
    , top
    , pop
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Stack a = Empty | Node a (Stack a)

empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _     = False

push :: a -> Stack a -> Stack a
push x s = Node x s

top :: Stack a -> a
top Empty    = error "top on empty stack"
top (Node x _) = x

pop :: Stack a -> Stack a
pop Empty    = error "pop on empty stack"
pop (Node _ s) = s

instance (Eq a) => Eq (Stack a) where
  Empty == Empty = True
  (Node x s) == (Node x' s') = x == x' && s == s'
  _ == _ = False

instance (Show a) => Show (Stack a) where
    show s = "LinearStack(" ++ intercalate "," (aux s) ++ ")"
        where
        aux Empty = []
        aux (Node x s) = show x : aux s