-------------------------------------------------------------------------------
-- Linear implementation of Sets with nodes sorted according to values
-- and non-repeated elements
--
-- Data Structures. Grado en Informática. UMA.
--
-- STUDENT'S NAME:
-------------------------------------------------------------------------------

module DataStructures.Set.SortedLinearSet 
  ( Set
  , empty
  , isEmpty
  , size
  , insert
  , isElem
  , delete

  , fold

  , union
  , intersection
  , difference  
  ) where

import Data.List(intercalate)
import Test.QuickCheck

-- Invariants for this data structure:
--  * (INV1) All Nodes store different elements (no repetitions)
--  * (INV2) Nodes are sorted in ascending order with 
--           respect to values of their elements
--
-- An example of a well constructed set:
--   Node 2 (Node 5 (Node 8 Empty))
--
-- Examples of wrong sets:
--   Node 2 (Node 5 (Node 5 (Node 8 Empty))) -- REPETITION OF ELEMENT 5!
--   Node 7 (Node 1 (Node 8 Empty)) -- ELEMENTS NOT IN ASCENDING ORDER!

data Set a  = Empty | Node a (Set a)

empty :: Set a
empty = Empty

isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False

isElem :: (Ord a) => a -> Set a -> Bool
isElem a Empty = False
isElem a (Node x s) | a < x     = False
                    | a == x    = True
                    | otherwise = isElem a s

insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = (Node x Empty)
insert x set@(Node y s)
  | x < y     = (Node x set)
  | x == y    = set
  | otherwise = (Node y (insert x s))

delete :: (Ord a) => a -> Set a -> Set a
delete x Empty = Empty
delete x (Node y s)
  | x < y     = (Node y s)
  | x == y    = s
  | otherwise = (Node y (delete x s))

size :: Set a -> Int
size Empty = 0
size (Node x s) = 1 + size s

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z = fun
 where
  fun Empty       = z
  fun (Node x s)  = f x (fun s)

union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = fold insert s2 s1

difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = fold delete s1 s2

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = fold intersect empty s1
  where
    intersect x s = if isElem x s2 then insert x s else s




-- Showing a set
instance (Show a) => Show (Set a) where
  show s  = "SortedLinearSet(" ++ intercalate "," (strings s) ++ ")"
    where
      strings Empty       = []
      strings (Node x s)  = show x : strings s

-- Set equality
instance (Eq a) => Eq (Set a) where
  Empty      == Empty         = True
  (Node x s) == (Node x' s')  = x==x' && s==s'
  _          == _             = False

-- This instance is used by QuickCheck to generate random sets
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)

