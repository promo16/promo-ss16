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

-- Ähnlich wie bei laenge.hs wird hier gleichermaßen das Ergebnis von vorne drangehängt
-- und der letzte rekursive Aufruf lässt folgendes beim Aufruf von rElem 3 [1,2,3] entstehen:

--    3 == 1 || 2 == 3 || 3 == 3 || False

-- | Endrekursiv
--

-- Bei den folgenden zwei Beispielen haben wir keine Hilfsfunktion - aber warum?
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

  Hier haben wir beim letzten rekursiven Aufruf sofort das Ergebnis rausbekommen! Wenn wir mit einer
  Funktion arbeiten die jederzeit abbrechen kann und das Ergebnis liefern kann, braucht es keine Hilfsfunktion!

-}

tElem' :: Eq a => a -> [a] -> Bool
tElem' x []     = False
tElem' x (y:ys) = if x == y 
                      then True
                      else tElem' x ys

-- Natürlich können wir es trotzdem hinschreiben. 
-- Die Laufzeit wird dadurch dennoch schlechter, weil der Overhead ein Bit (0/1) durchzureichen zu groß
-- ist.
tElem'' :: Eq a => a -> [a] -> Bool
tElem'' x ys = go False x ys
    where
        go :: Eq a => Bool -> a -> [a] -> Bool
        go !b x     [] = b
        go !b x (y:ys) = go (b || x == y) x ys

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