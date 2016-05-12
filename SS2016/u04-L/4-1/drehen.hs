-- Für Benutzung, Installation sowie der Erklärung zu criterion -> laenge.hs
--
--
-- 4-1 c) eine Liste drehen --

{-# LANGUAGE BangPatterns #-}

import Criterion.Main

-- | Nicht endrekursiv
--
rRev :: [a] -> [a]
rRev []     = []
rRev (x:xs) = rRev xs ++ [x]

-- Beispiel: rRev [1,2,3]

{-
   rRev [1,2,3]
=> rRev (1:[2,3]) = rRev [2,3] ++ [1]
=> rRev (2:[3])   = rRev [3]   ++ [2] ++ [1]
=> rRev (3:[])    = rRev []    ++ [3] ++ [2] ++ [1]  (wir wissen das Ergebniss noch nicht)

Dafür ist folgender Schritt nötig:

=>                       []    ++ [3] ++ [2] ++ [1]
=> [3,2,1]
-}

-- | Endrekursiv
--
tRev :: [a] -> [a]
tRev xs = go [] xs
    where
        go :: [a] -> [a] -> [a]
        go acc []     = acc
        go acc (x:xs) = go (x:acc) xs

-- Beispiel: tRev [1,2,3]
{-
   tRev [1,2,3]
=> go []    [1,2,3]
=> go []    (1:[2,3]) = go (1:[])   [2,3]
=> go [1]   (2:[3])   = go (2:[1])   [3]
=> go [2,1] (3:[])    = go (3:[2,1]) []
=> go [3,2,1] []      = [3,2,1]
-}

main :: IO ()
main = defaultMain [
    bgroup "recursive" [ bench "rRev whnf" $ whnf rRev ([1..10000] :: [Int])
                       , bench "rRev nf"   $ nf   rRev ([1..10000] :: [Int])
                       ]
  , bgroup "tail-recursive" [ bench "tRev 1 whnf" $ whnf tRev ([1..10000] :: [Int])
                            , bench "tRev 1   nf" $   nf tRev ([1..10000] :: [Int])
                            ]
    ]

-- Interpretation wird live gemacht, da sie stark abweichen kann. Ist eh nur eine schöne Visualisierung
-- damit man sich merken kann, dass TCO (tail-call-optimizaion) was taugt.