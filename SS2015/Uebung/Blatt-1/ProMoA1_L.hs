{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Prelude hiding (tail)
import Data.List (sort)

-- A1-1

-- Gesucht: Ergebnis

-- a)

a1_a :: [(Int, [Char])]
a1_a = [(x,y) | x <- [11,7], y <- ["clubs", "hearts"]]
-- > [(11,"clubs"),(11,"hearts"),(7,"clubs"),(7,"hearts")]

-- b)

a1_b :: [(Int, Int, Int)]
a1_b = [(x,y,z) | (x,y) <- list, z <- [5,6]]
  where list = [(a,b) | a <- [1,2], b <- [3,4]]
-- > [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]

-- c)

a1_c :: [(Int, Int, Int)]
a1_c = [(x,y,z) | (y,z) <- list, x <- [1,2]]
  where list = [(a,b) | a <- [3,4], b <- [5,6]]
-- > [(1,3,5),(2,3,5),(1,3,6),(2,3,6),(1,4,5),(2,4,5),(1,4,6),(2,4,6)]

-- d)

a1_d :: [(Int, Int)]
a1_d = [(x,y) | x <- [1,2], y <- []]
-- > []

-- e)

a1_e :: Int -> Int
a1_e n = length [(x,y) | x <- [1..n], y <- [27,69]]
-- > 2*n

-- A1-2

-- Gesucht: Typ

-- a)
a2_a :: [Char]
a2_a = 'c':'o':'o':'l':"!"
-- > "cool!"

-- b)

a2_b :: [Bool]
a2_b = (4 == 5.0):[]
-- > [False]

-- c)

a2_c :: Bool
a2_c = let mond="käse" in
  if mond=="käse" && 1==2 then False
                          else 1+1==2
-- > True

-- d)

a2_d :: [Char]
a2_d = [z | c <- "grotesk", c/='k', c/='r', c/='e' && c/='s', let z = if c=='o' then 'u' else c]
-- > "gut"

-- e)

a2_e :: [(Int, Int)]
a2_e = [(a,b) | a <- [1..5], b <- [5..1], a/=b]
-- > []

-- f)

a2_f :: String
a2_f = (\x -> "nope!") "aaa"
-- > "nope!"

-- A1-3

-- Gesucht: Vereinfache die Funktion
tail :: [a] -> [a]
tail (_:ys) = ys

head :: [a] -> a
head (x:_) = x

-- [n] == n:[]

a3       = ((\x -> (\(y:_) -> x - y)) (3+4)) (tail [1,2])

-- ab hier
a3'      = ((\x -> (\(y:_) -> x - y)) (3+4)) 2:[]

a3''     = ((\x -> (\(y:_) -> x - y)) 7) 2:[]

a3'''    = ((\(y:_) -> 7 - y) 2:[])

-- bis hier nicht ausführbar, da keine Typannotation vorhanden ist

a3'''''  = 7 - 2

a3'''''' = 5


-- A1-4

-- Gesucht: Implementiere das logische UND...

-- a) ...nur mit Pattern Matching

myAnd_a :: Bool -> (Bool -> Bool)
myAnd_a True  True  = True
myAnd_a False True  = False
myAnd_a True  False = False
myAnd_a False False = False

myAnd_a' :: Bool -> (Bool -> Bool)
myAnd_a' True b = b
myAnd_a' _ _    = False

-- b) ... nur mit Guards

myAnd_b :: Bool -> (Bool -> Bool)
myAnd_b x y
  | x, y    = True
  | otherwise = False

myAnd_b' :: Bool -> (Bool -> Bool)
myAnd_b' x y
  | x && y    = True
  | otherwise = False

-- c) ...ohne Guards und Pattern Matching

myAnd_c :: Bool -> (Bool -> Bool)
myAnd_c x y = if x then y else False


main :: IO()
main = do
  putStrLn $ show $ myAnd_a True True