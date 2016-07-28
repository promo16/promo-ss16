
-- Aufgabe 2-3 --

--a) ungeraden Elemente einer Liste -- 
a :: Integral a => [a] -> [a]
a xs = [x | x <- xs, x `mod` 2 == 1]

a'  xs = [x | x <- xs, odd x]

a'' xs = [x | x <- xs, not (even x)]
 
--b) geraden Elemente einer Liste -- 
b :: Integral a => [a] -> [a]
b xs = [x | x <- xs, x `mod` 2 == 0]

b'  xs = [x | x <- xs, even x]

b'' xs = [x | x <- xs, not (odd x)]

--c) Laenge einer Liste -- 
c :: [a] -> Int
c xs = sum [1 | _ <- xs]

--Hier war die Idee dass man ein Element basierend auf einer Zahl/Länge der Liste zur Liste hinzufügt.

--Das können List-Comprehentions sehr gut - sie haben immer den Ausdruck links vor dem | den sie zur Liste hinzufügen und rechts davon stehen immer die Bedingungen.

--Im Fall c) ist die Bedinung ^= "sooft wie die Liste lang ist"

--len xs = [1 | x <- xs]

--Jetzt fügen wir die Zahl 1 sooft hinzu wie es Elemente gibt.
--Nun müssen wir sie nur noch aufsummieren, dafür gibt es die Funktion sum.

--len xs = sum [1 | x <- xs]

--Wir sehen aber, dass das 'x' in unserem Ausdruck gar nicht gebraucht wird! Deswegen ersetzen wir ihn mit einer sog. Wildcard. Das bedeutet einfach dass das Programm keinen Speicher allokiert für diesen Wert.

--len xs = sum [1 | _ <- xs]


--d) Liste mit n Leerzeichen -- 
d :: Integral a => a -> [Char]
d n = [' ' | x <- [1..n]]

d' n = [' ' | _ <- [1..n]]

d'' :: Integral a => a -> [String]
d'' n = [" " | x <- [1..n]]

-- Erklärung:
--
--     (gelesen) Füge das Element ' ' zur Liste hinzu pro Element in der Liste [1..n]
--     Man kann das x durch einen Unterstrich (sog. Wildcard) ersetzen, wenn man diese Variable
--     nicht benutzt.

--e) natürlichen Zahlen zwischen 7 und 77 mit Rest 5 bei der Division durch 7 -- 
e :: Integral a => [a]
e = [x | x <- [7..77], x `mod` 7 == 5]


--f) dreifach -- 
f :: Integer -> Integer
f x = [y * 3 | y <- [x]] !! 0

f'   x = head [y * 3 | y <- [x]]

f''  x = [y * 3 | y <- [1..x]] !! (x - 1)

f''' x = last [y * 3 | y <- [1..x]]

-- Erklärung: Diese Funktion war nicht wirklich durchdacht...

-- Man beachte dass nur die ersten beiden Definition nicht fehlschlagen können:

-- f''  (-1) => *** Exception: Prelude.!!: negative index
-- f''' (-1) => *** Exception: Prelude.last: empty list

--g) nur Großbuchstaben ausgeben eines Strings -- 
g :: [Char] -> [Char]
g s = [x | x <- s, x `elem` ['A'..'Z']]


-- Erklärung:
--   Hier können wir ausnutzen dass wie eine Liste mit allen Großbuchstaben erstellen können:

--  ['A'..'Z']

--  Nun müssen wir nur noch überprüfen ob jedes Char in unserem String in dieser Liste ist und falls ja, geben wir ihn zurück.

--h) Faktorzerlegung eines Integers -- 
h :: Integer -> [Integer]
h n = [x | x <- [2..(n-1)], n `mod` x == 0]

-- Erklärung:
--  Die Idee hier war einfach Zahlen zu suchen die ohne Rest x teilen, also wenn n modulo x = 0.
--  Das sind die Primfaktoren. 
--  Wir haben die Grenzen von 2 - (n-1) gesetzt, weil die 1, sowie die Zahl selber nicht zu den Primfaktoren gehört.
      
--i) Pythagoras Trippel --
i :: Integer -> [(Integer, Integer, Integer)]
i n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- ohne Duplikate
i' n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]