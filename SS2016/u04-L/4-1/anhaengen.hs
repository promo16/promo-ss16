-- Für Benutzung, Installation sowie der Erklärung zu criterion -> laenge.hs
--
--
-- 4-1 d) zwei Listen aneinanderhaengen --

{-# LANGUAGE BangPatterns #-}

import Criterion.Main

-- | Nicht Endrekursiv
--
rApp :: [a] -> [a] -> [a]
rApp []     ys = ys
rApp xs     [] = xs
rApp (x:xs) ys = x : rApp xs ys

-- Beispiel:

{-
   rApp [1,2,3]   [4,5,6]
=> rApp (1:[2,3]) [4,5,6] = 1 : rApp [2,3] [4,5,6]
=> rApp (2:[3])   [4,5,6] = 1 : 2 : rApp [3] [4,5,6]
=> rApp (3:[])    [4,5,6] = 1 : 2 : 3 : rApp [] [4,5,6]
=> rApp []        [4,5,6] = 1 : 2 : 3 : [4,5,6]   (wir wissen das Ergebniss noch nicht)

Dafür ist folgender Schritt erst nötig:

=> 1 : 2 : 3 : [4,5,6]
=> [1,2,3,4,5,6]

-}

-- | Endrekursiv
--
tApp :: [a] -> [a] -> [a]
tApp xs ys = go ys (reverse xs)
    where go :: [a] -> [a] -> [a]
          go ys []     = ys
          go ys (x:xs) =  go (x:ys) xs

-- Beispiel:
--
{-
   tApp [1,2,3] [4,5,6]
=> go   [4,5,6] (reverse [1,2,3])

=> go   [4,5,6]       [3,2,1]
=> go   [4,5,6]       (3:[2,1]) = go (3:[4,5,6]) [2,1]
=> go   [3,4,5,6]     (2:[1])   = go (2:[3,4,5,6]) [1]
=> go   [2,3,4,5,6]   (1:[])    = go (1:[2,3,4,5,6]) []
=> go   [1,2,3,4,5,6] []        = [1,2,3,4,5,6]

-}

main :: IO ()
main = defaultMain [
    bgroup "recursive" [ bench "rApp whnf" $ whnf (rApp ([1..10000] :: [Int])) ([10000,9999..1] :: [Int])
                       , bench "rApp nf"   $ nf   (rApp ([1..10000] :: [Int])) ([10000,9999..1] :: [Int])
                       ]
  , bgroup "tail-recursive" [ bench "tApp whnf" $ whnf (tApp ([1..10000] :: [Int])) ([10000,9999..1] :: [Int])
                            , bench "tApp   nf" $   nf (tApp ([1..10000] :: [Int])) ([10000,9999..1] :: [Int])
                            ]
    ]

-- Interpretation wird live gemacht, da sie stark abweichen kann. Ist eh nur eine schöne Visualisierung
-- damit man sich merken kann, dass TCO (tail-call-optimizaion) was taugt.
