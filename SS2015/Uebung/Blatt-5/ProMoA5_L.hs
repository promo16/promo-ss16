module Main where

import Prelude hiding (dropWhile, all)

-- | A5-1

-- a)

foo1 :: (Ord a, Num a) => (a -> b) -> (a -> Bool) -> [a] -> [b]
foo1 f p xs = [f x | x <- xs, x >= 0, p x]

foo2 :: (Ord a, Num a) => (a -> b) -> (a -> Bool) -> [a] -> [b]
foo2 f p xs = map f (filter p (filter (\x -> x >= 0) xs))

foo3 :: (Ord a, Num a) => (a -> b) -> (a -> Bool) -> [a] -> [b]
foo3 f p xs = map f (filter (\x -> x >= 0 && p x) xs)

-- '$' kann die äußerste Klammer ersetzen!
foo4 :: (Ord a, Num a) => (a -> b) -> (a -> Bool) -> [a] -> [b]
foo4 f p xs = map f $ filter (\x -> x >= 0 && p x) xs


e_1 = foo1 (show) (even) [0..10]
e_2 = foo2 (show) (even) [0..10]
e_3 = foo3 (show) (even) [0..10]
e_4 = foo4 (show) (even) [0..10]


-- b)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f xs@(x:xs')
  | f x        = dropWhile f xs'
  | otherwise  = xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f (x:xs)
  | f x = dropWhile' f xs
dropWhile' _ list = list

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' f list = acc list
  where
    acc (x:xs)
      | f x  = acc xs
    acc list = list

-- im point-free style
dropWhile''' :: (a -> Bool) -> [a] -> [a]
dropWhile''' f = acc
  where
    acc (x:xs)
      | f x  = acc xs
    acc list = list

b_1 = dropWhile    (\x -> (length x) < 2) ["0","2","4","6","8","10"]
b_2 = dropWhile'   (\x -> (length x) < 2) ["0","2","4","6","8","10"]
b_3 = dropWhile''  (\x -> (length x) < 2) ["0","2","4","6","8","10"]
b_4 = dropWhile''' (\x -> (length x) < 2) ["0","2","4","6","8","10"]


curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a,b)

-- c)

all :: (a -> Bool) -> [a] -> Bool
all f xs = (and . map f) xs

-- point-free style
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

all'' :: (a -> Bool) -> [a] -> Bool
all'' f xs = foldl (\ys y -> f y && ys) True xs

-- point-free style
all''' :: (a -> Bool) -> [a] -> Bool
all''' f = foldl (\ys y -> f y && ys) True

-- | A5-2

-- a)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

uncurry3 :: (a ->  b ->  c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

-- b)

a_1 = uncurry3            foldr   ((+), 0,  [1..5])
a_2 = uncurry (uncurry    foldr) (((+), 0), [1..5])
a_3 = (uncurry . uncurry) foldr  (((+), 0), [1..5])


-- kurze Erklärung von foldl / foldr / map

--                Funktion
--            _______|________
--           /                \
--        acc     Element    acc'   Startelement  Startliste
--           |       |        |        |              |
--           |       |        |        |              |
-- foldl :: (b    -> a     -> b)    -> b          -> [a] -> b
-- foldl    (\xs     x     -> xs ++ x) []      ["a", "b", "c"]

c_1 = foldl (\xs     x     -> xs ++ x) ""      ["a", "b", "c"]
-- "abc"

-- foldl fängt mit dem ersten Element der Liste führt die Funktion aus,
-- geht zum nächsten Element und geht damit von links nach rechts
-- durch die Liste durch

-- foldl (..) [] ["a", "b", "c"]
-- foldl (..) ([] ++ "a") ["b", "c"]
-- foldl (..) ([] ++ "a" ++ "b") ["c"]
-- foldl (..) ([] ++ "a" ++ "b" ++ "c") []
-- [] ++ "a" ++ "b" ++ "c"
-- "abc"

--                Funktion
--            _______|________
--           /                \
--       Element     acc     acc'    Startelement  Startliste
--           |       |        |         |              |
--           |       |        |         |              |
-- foldr :: (a    -> b     -> b)     -> b          -> [a] -> b
-- foldr    (\x      xs     -> x ++ xs) []      ["a", "b", "c"]

c_2 = foldr (\x      xs     -> x ++ xs) []      ["a", "b", "c"]
-- foldr geht bis zum Ende der Liste durch führt die aufsummierten
-- Funktionen von rechts nach links aus!

-- [] ++ (foldr (..) ["a", "b", "c"])
-- [] ++ "a" ++ (foldr (..) ["b", "c"])
-- [] ++ "a" ++ "b" ++ (foldr (..) ["c"])
-- [] ++ "a" ++ "b" ++ "c"
-- "abc"

-- Die Funktion wir auf jedes Element der Liste angewendet
--
--     Funktion   Startliste
--          |         |
--map :: (a -> b) -> [a] -> [b]

c_3 = map (+1) [0..5]
c_4 = map (\x -> x + 1) [0..5]