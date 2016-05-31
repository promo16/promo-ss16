-- Für Benutzung, Installation sowie der Erklärung zu criterion -> laenge.hs
--
--
-- Bei mehreren Argumenten (wie in diesem Beispiel) müsst ihr die Funktion und alle Argumente bis
-- auf das Letzte klammern
--
--
{-# LANGUAGE BangPatterns #-}

import Criterion.Main

-- 4-1 b) ist Element in Liste --

-- | Nicht endrekursiv
--
rElem :: Eq a => a -> [a] -> Bool
rElem x []     = False
rElem x (y:ys) = x == y || rElem x ys

-- Hier ist der letzte rekursive Aufruf '(3 == 3) || ...' , wenn man die vorherigen Schritte für 1 und 2 durchläuft.

-- VERBESSERUNG (!!)
--
tElem :: Eq a => a -> [a] -> Bool
tElem x []     = False
tElem x (y:ys)
  | x == y    = True
  | otherwise = tElem x ys

{- 
   tElem 3 [1,2,3]
=> tElem 3 (1:[2,3])
     | 1 == 3
  => | otherwise = tElem 3 [2,3]

=> tElem 3 (2:[3])
     | 2 == 3 
  => | otherwise = tElem 3 [3]

=> tElem 3 (3:[])
     | 3 == 3 => True

=> True

-- Diese Lösung auch nicht endrekursiv. Wenn man genau hinschaut, wird bei dem letzten rekursiven Aufruf lediglich
-- ein Guard abgefragt, der widerrum (3 == 3) abfragt. Damit stand das Ergebnis beim letzten Aufruf nicht fest
-- => NICHT endrekursiv

-}


-- Analog zur Guard - Lösung (nicht endrekursiv)
--
tElem' :: Eq a => a -> [a] -> Bool
tElem' x []     = False
tElem' x (y:ys) = if x == y 
                      then True
                      else tElem' x ys


-- Endrekursiv
--
-- Die Laufzeit wird dadurch aber leider schlechter, weil der Overhead ein Bit (0/1) durchzureichen zu groß
-- ist.
tElem'' :: Eq a => a -> [a] -> Bool
tElem'' x ys = go False x ys
    where
        go :: Eq a => Bool -> a -> [a] -> Bool
        go !b x     [] = b
        go !b x (y:ys) = go (b || x == y) x ys


-- Endrekursiv
--
-- foldl ist eine Funktion die immer endrekursiv ist. (mehr zu der Erklärung im Blatt 8)
--
tElem''' :: Eq a => a -> [a] -> Bool
tElem''' e l = foldl (\xs x -> e == x || xs) False l

main :: IO ()
main = defaultMain [
    bgroup "recursive" [ bench "nElem whnf" $ whnf (rElem 100000) [1..100000]
                       , bench "nElem nf"   $ nf   (rElem 100000) [1..100000]
                       ]
  , bgroup "tail-recursive" [ bench "tElem 1 whnf" $ whnf (tElem   100000) [1..100000]
                            , bench "tElem 1   nf" $   nf (tElem   100000) [1..100000]
                            , bench "tElem 2 whnf" $ whnf (tElem'  100000) [1..100000]
                            , bench "tElem 2   nf" $   nf (tElem'  100000) [1..100000]
                            , bench "tElem 3 whnf" $ whnf (tElem'' 100000) [1..100000]
                            , bench "tElem 3   nf" $   nf (tElem'' 100000) [1..100000]
                            ]
    ]

-- Interpretation wird live gemacht, da sie stark abweichen kann. Ist eh nur eine schöne Visualisierung
-- damit man sich merken kann, dass TCO (tail-call-optimizaion) was taugt.