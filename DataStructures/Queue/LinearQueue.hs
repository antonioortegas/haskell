-------------------------------------------------------------------------------
-- Linear implementation of Sets with nodes sorted according to values
-- and non-repeated elements
--
-- Data Structures. Grado en Informática. UMA.
--
-- STUDENT'S NAME:
-------------------------------------------------------------------------------

module DataStructures.Queue.LinearQueue
    ( Queue
    , empty
    , isEmpty
    , enqueue
    , first
    , dequeue
    ) where

import Data.List(intercalate)
import Test.QuickCheck

data Queue a = Empty | Node a (Queue a) deriving Show

empty :: Queue a
empty = Empty

isEmpty :: Queue a -> Bool
isEmpty Empty = True
isEmpty _     = False

enqueue :: a -> Queue a -> Queue a
enqueue x Empty = Node x Empty
enqueue x (Node y q) = Node y (enqueue x q)

first :: Queue a -> a
first Empty = error "first on empty queue"
first (Node x _) = x

dequeue :: Queue a -> Queue a
dequeue Empty = error "dequeue on empty queue"
dequeue (Node _ q) = q