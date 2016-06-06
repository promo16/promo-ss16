-- Aufgabe 8-3 Eigene Funktionen höherer Ordnung
--

import Prelude hiding (zipWith)

-- a) implementieren sie zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--
-- Diese Funktion kann oft nützlich sein, wenn man Listen auf die gleiche Länge bringen will (und wenn man mit unendlichen Listen arbeitet)
--
-- fibs = 0 : 1 : zipWith (++) (tail fibs) fibs
-- [0,1,1,2,3,5,8,13...]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f  xs     []    = []
zipWith f  []     ys    = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- zipWith (+) [1,2,3] [4,5,6]
-- 
-- => zipWith (+) (1:[2,3]) (4:[5,6]) = 1 + 4 : zipWith (+) [2,3] [5,6]
-- 
-- => 1 + 4 : zipWith (+) (2:[3]) (5:[6]) = 2 + 5 : zipWith (+) [3] [6]
-- 
-- => 1 + 4 : 2 + 5 : zipWith (+) (3:[]) (6:[]) = 3 + 6 : zipWith (+) [] []
-- 
-- => 1 + 4 : 2 + 5 : 3 + 6 : []
-- 
-- => 5 : 7 : 9 : []
-- 
-- => [5, 7, 9]


-- b) implemtenieren sie unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
--
-- eine etwas ungewöhnliche Funktion, die vom Typ nicht offensichtlich ableitbar ist
-- Die Idee war auf jedes Element aus der [a] Liste die Funktion anzuwenden und dann
-- jeweils das erste Argument vom Tupel in eine einzelne Liste zu packen, genauso das zweite Argument
-- 
--
-- unzipWith (\x -> (x+1, x+2)) [1..3]
-- ([1,2,3], [3,4,5])

unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f l = go l ([], [])
    where go []     acc = acc
          go (x:xs) (ys, zs) = let (y, z) = f x
                               in go xs (y:ys, z:zs)

unzipWith' :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith' f l = go l ([], [])
    where go []     acc = acc
          go (x:xs) (ys, zs) = go xs (y:ys, z:zs)
              where (y, z) = f x

unzipWith'' :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith'' f l = go l ([], [])
    where go []     acc = acc
          go (x:xs) (ys, zs) = go xs ((fst (f x)):ys, ((snd (f x)):zs))

-- Mit unserem neu gewonnen Wissen von folds, können wir foldr für die Definition benutzen
--
unzipWith''' :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith''' f l = foldr g ([], []) l
    where g e (xs, ys) = ((fst (f e)) : xs, (snd (f e)) : ys)

unzipWith'''' :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith'''' f l = foldr g ([], []) l
    where g e (xs, ys) = let (x, y) = f e in (x:xs, y:ys)

-- Meiner Meinung nach die einfachste Definition
--
unzipWith''''' :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith''''' f l = (map (fst . f) l, map (snd . f) l)

--  *) 'fst' gibt mir das erste Element eines Tupels zurück
--  *) 'snd' gibt mir das zweite Element eines Tupels zurück
--  *) 'fst . f' wendet 'fst' auf das Ergebnis von 'f' an, nachdem ihr ein Argument gegeben wurde
--  *) 'map (fst . f) l' wendet auf jedes Element aus der Liste 'l' die Funktion 'fst . f' an

-- Beispieldurchlauf:

--     unzipWith id [(0, 'g'), (8, 'u'), (9, 't')]
-- 
-- =>  (map (fst . id) [(0, 'g'), (8, 'u'), (9, 't')], map (snd . id) [(0, 'g'), (8, 'u'), (9, 't')])
-- 
-- =>      map (fst . id) [(0, 'g'), (8, 'u'), (9, 't')]
-- 
-- =>      [(fst . id) (0, 'g'), (fst . id) (8, 'u'), (fst . id) (9, 't')]
-- 
-- =>      [fst (0, 'g'), fst (8, 'u'), fst (9, 't')]          -- id verändert das Element nicht
-- 
-- =>      [0, 8, 9]                                           -- fst gibt uns das erste Element des Tuples zurück
-- 
-- 
-- => ([0, 8, 9],  map (snd . id) [(0, 'g'), (8, 'u'), (9, 't')]
-- 
-- =>      map (snd . id) [(0, 'g'), (8, 'u'), (9, 't')]
-- 
-- =>      [(snd . id) (0, 'g'), (snd . id) (8, 'u'), (snd . id) (9, 't')]
-- 
-- =>      [snd (0, 'g'), snd (8, 'u'), snd (9, 't')]          -- id verändert das Element nicht
-- 
-- =>      ['g', 'u', 't']                                     -- snd gibt uns das zweite Element des Tuples zurück
-- 
-- => ([0, 8, 9], ['g', 'u', 't'])  -- oder auch ([0, 8, 9], "gut")

