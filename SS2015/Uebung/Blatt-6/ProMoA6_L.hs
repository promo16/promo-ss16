module Main where


-- | A6-1

compose :: [(a -> a)] -> a -> a
compose = foldr (.) id

compose' :: [(a -> a)] -> a -> a
compose' fs = foldr (\xs x -> xs . x) id fs

--geradesumme = compose [sum, map (^2), filter even]

-- :t sum :: (Num a) => [a] -> a

-- :t map (^2) :: [a] -> [a]

-- :t filter even :: (Integral a) => [a] -> [a]

-- Eine Liste muss immer Elemente mit dem gleichen Typ haben!

geradesumme = sum . map (^2) . filter even

geradesumme' = sum . compose [map (^2), filter even]


-- | A6-2

a = div 169 $ 3 + 1

b = sum $ filter even $ [3..11] ++ [13..15]

c = ($ 2) (\x -> x * 21)

-- :t ($) :: (a -> b) -> a -> b

-- :t ($ 2) :: (Num a) => (a -> b) -> b

-- Prefix Schreibweise für $

d = (foldr) ($ 6) (6) [(-), (*), (-), (+)]

-- | A6-3

data Tree a = Empty | Node { label :: a, left, right :: Tree a }
  deriving Show

leaf :: a -> Tree a
leaf a = Node a Empty Empty

myTree :: Tree Int
myTree = Node 69 (leaf 77)
                 (Node 44  (leaf 64)
                           (leaf 7))


myFmap :: (a -> b) -> Tree a -> Tree b
myFmap _ Empty        = Empty
myFmap f (Node x l r) = Node (f x) (myFmap f l) (myFmap f r)


instance Functor Tree where
    fmap f Empty = Empty
    fmap f Node {left = l        , label = x   , right = r        } =
           Node {label = f x , right = fmap f r, left = fmap f l  }