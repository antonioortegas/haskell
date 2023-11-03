data Tree a = Empty | Node a [Tree a] deriving Show

tree1:: Tree Int
tree1 = Node 1 [Node 2 [
                    Node 4 [],
                    Node 5 [],
                    Node 6 []
                ],
                Node 3[
                    Node 7[]
                ]
                ]

sizeT :: Tree a -> Int
sizeT Empty = 0
--Para hacerlo con listas por comprension
--sizeT (Node x ts) = 1 + sum [sizeT t | t <- ts]
--Para hacerlo con map
sizeT (Node x ts) = 1 + sum (map sizeT ts)

sumT :: (Num a) => Tree a -> a
sumT Empty = 0
sumT (Node x ts) = x + sum (map sumT ts) 