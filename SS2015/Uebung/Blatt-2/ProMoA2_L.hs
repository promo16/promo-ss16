module Main where

import Prelude hiding (replicate)

-- A2-1

-- a)

-- | Gesucht: Die Funktion replicate die folgendes macht
--   replicate 3 "hello" -> ["hello", "hello", "hello"]

-- | Mit Guards

replicate :: Int -> a -> [a]
replicate n element
  | n >= 0    = element : replicate (n-1) element
  | otherwise = []

-- | Ohne Guards

replicate' :: Int -> a -> [a]
replicate' 0 _       = []
replicate' n element = element : replicate' (n-1) element

-- b)

-- | Gesucht: Endrekursive Version von replicate

replicate'' :: Int -> a -> [a]
replicate'' n element = myReplicate n []
  where
        myReplicate 0 accumulator = accumulator
        myReplicate n accumulator = myReplicate (n-1) (element:accumulator)

-- | Was ist Endrekursion: Eine Funktion ist dann endrekursiv / tail-recursive
--   wenn der letzte Funktionsaufruf rekursiv ist.

-- c)

-- | Gesucht: Replicate mit List-Comprehentions

replicate''' :: Int -> a -> [a]
replicate''' n element = [element | _ <- [1..n]]

-- | Bemerkung: Nicht jede rekursive Funktion lässt sich mithilfe einer List-Comprehention
--   ausdrücken, umgekehrt gilt dies schon, da Rekursion allgemeiner ist.


-- A2-2

-- a) Implementieren sie den Quicksort-Algorithmus mithilfe von splitBy

quicksort :: [Int] -> [Int]
quicksort []     = []
quicksort (l:ls) = quicksort smaller ++ l : quicksort bigger
  where (smaller, bigger) = splitBy l ls

-- b) Implementieren sie splitBy

-- | Mit List-Comprehentions (nicht besonders effizient)
splitBy :: Int -> [Int] -> ([Int],[Int])
splitBy n list = (smallerThan, biggerThan)
  where smallerThan, biggerThan :: [Int]
        smallerThan = [x | x <- list, x <= n]
        biggerThan  = [x | x <- list, x > n]

-- | Rekursive Variante
splitBy' :: Int -> [Int] -> ([Int], [Int])
splitBy' _ []     = ([], [])
splitBy' n (l:ls)
    | n >= l    = (l : smaller,     bigger)
    | otherwise = (    smaller, l : bigger)
  where (smaller, bigger) = splitBy' n ls


-- | Endrekursive Variante
splitBy'' :: Int -> [Int] -> ([Int], [Int])
splitBy'' pivot list = splitByAcc ([], []) list
  where splitByAcc :: ([Int], [Int]) -> [Int] -> ([Int], [Int])
        splitByAcc acc               []     = acc
        splitByAcc (smaller, bigger) (l:ls)
            | l <= pivot = splitByAcc (l : smaller,     bigger) ls
            | otherwise  = splitByAcc (    smaller, l : bigger) ls


-- A2-3

tops :: [Int]
tops = [0,0,0,1,1,1,2,2,2,3,3,3,3]

hosen :: [Int]
hosen = [0,0,0,0,1,1,1,2,2,3]

schuhe :: [Int]
schuhe = [0,0,0,1,1,2,3]

miasKombinationen :: [(Int, Int, Int)]
miasKombinationen = [(t,h,s) | t <- tops, h <- hosen, s <- schuhe, t /= h, t == s || h == s]

anzahlVonKombinationen :: Int
anzahlVonKombinationen = length miasKombinationen
  -- 365