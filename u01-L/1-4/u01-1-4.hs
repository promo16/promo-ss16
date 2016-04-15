{- 

Bei diesen Lösungen muss man leider differenzieren, ob man sie in
einem File in den Interpreter lädt, oder ob man diese im GHCi direkt
reintippt.

Der Interpreter versucht immer die polymorphsten Typen zu inferieren,
wenn man jedoch ein File reinlädt inferiert ist er nicht so großzügig.

Deswegen gibt es immer zwei verschiedene Lösungen,

File reinladen <-> F:
Direkt in GHCi <-> G:

In der Klausur wird sicherlich der abstrakteste Typ gefragt, also die G: Lösung

-}

-- a)
a = [1,2,3,4] ++ [9,10,11,12] -- [1,2,3,4,9,10,11,12]
-- F: [Integer]
-- G: Num a => [a]


b = [3,4,2] > [2,4]
-- [3,4,2] > [2,4]
-- 3       > 2
-- True 

-- F: Bool
-- G: Bool

-- Erklärung:
--     Bei solchen Vergleichen wird immer das erste Element genommen.
--     Wenn das erste Element keine Aussage zulässt, schauen wir auf das
--     zweite Element u.s.w

--  Beispiel:

--  [3] > [2]  => 3 > 2 => True

--  [3] > [3]  => 3 > 3 => False

--  [3] > [2, 3] => 3 > 2 => True (der Rest wird ignoriert)


--  [3] == [3] => 3 == 3 => True
--  [3] == [3,2] ==> 3 == 3 => True => [] == [2] => False
--  Listen verschiedener Länge sind automatisch ungleich
 

-- c)
c = [1,2,3,4,5,6,7,8] !! 5  -- 6
-- F: Integer
-- G: Num a => a

-- Erklärung: (!!) hat die Typsignatur:
--
--         [a] -> Int -> a
--          \       \
--           \       \________________
--            \_________________      \_______
--                     |        |     |       |
-- das heißt sie nimmt eine Liste und ein Index und gibt dir das Element
-- an dem bestimmten Index zurück. Wie gewöhnt fängt der Index bei 0 an:

-- Index: 0 1 2 3 4 5 6 7
--       [1,2,3,4,5,6,7,8]
-- An dem Index 5 steht die Zahl 6.

-- Nice to know:
--     Diese Funktion ist partiell - d.h sie terminiert nicht immer.
--     Ein einfaches Beispiel dafür wäre:

-- [1,2,3,4] !! 10 => Fehler, da der Index 10 nicht existiert.


-- d)
d = null [1,2,3] -- False
-- F: Bool
-- G: Bool

-- Erklärung: Die Funktion null hat die folgende Typsignatur:

--     Foldable t => t a -> Bool
--
--  Wir ersetzen Foldable erstmal durch eine Liste, da es hier erstmal keinen Unterschied
--  spielt.

--     [a] -> Bool

-- das heißt sie nimmt eine Liste von egal welchem Typ und gibt uns ein Bool zurück!
-- Diese Funktion überprüft lediglich ob die Liste leer ist oder nicht.

-- Beispiel:
--
-- null [] => True
-- null [1] => False

-- e)
e = length [5,4,3,2,1] -- 5
-- F: Int
-- G: Int

-- f)
f = head [] -- *** Exception: Prelude.head: empty list
-- 
-- Erklärung: Die Funktion head hat die folgende Typsignatur:

--        [a] -> a

-- das heißt sie nimmt eine Liste von egal welchem Typ und gibt uns ein Element
-- desselben Typs zurück. Es ist immer das erste Element der Liste.

-- Diese Funktion ist eine sog. partielle Funktion da sie nicht immer terminiert.
-- In unserem Beispiel haben wir das gesehen - sie ist auf leeren Listen nicht definiert
-- und gibt eine Fehlermeldung.

-- Beispiel:

-- head [1,2,3]       =>  1
-- head ['a','b','c'] => 'a'
-- head "Hallo"       => 'H'

-- g)
g = tail [2,3,4] -- [3,4]
-- F: [Integer]
-- G: Num a => [a]

-- Erklärung: Die Funktion tail hat die folgende Typsignatur:

--        [a] -> [a]

-- das heißt sie nimmt eine Liste von egal welchem Typ und gibt uns widerrum eine Liste
-- desselben Typs zurück. Es ist immer die gleiche Liste, ohne das erste Element.


-- Diese Funktion ist auch partiell, da sie genauso wie head nicht auf leeren Listen definiert ist.

-- Beispiel:

-- tail []            => *** Exception: Prelude.tail: empty list
-- tail [1,2]         => [2]
-- tail ['a','b','c'] => ['b', 'c']
-- tail "hallo"       => "allo"


-- h)
h = 'H' : "-Milch" -- "H-Milch"
-- F: [Char]
-- G: [Char]

-- i)
i = "Professor Bry" !! 6 -- 's'
-- F: Char
-- G: Char

-- Erklärung:
--
--    0    1    2    3    4    5    6    7    8
--  ['P', 'r', 'o', 'f', 'e', 's', 's', 'o', 'r'...]

-- j)
kopf :: [a] -> a
kopf   xs = head xs

kopf'  xs = xs !! 0

kopf'' xs = [x | x <- xs] !! 0

-- Pattern Matching kommt später, aber dennoch gut die Beispiele mal gesehen zu haben
kopf''' (x:xs) = x

kopf'''' (x:_) = x


-- k)
ende :: [a] -> a
ende   xs = last xs

ende'  xs = xs !! ((length xs) - 1)

ende'' xs = [x | x <- xs] !! ((length xs) - 1)

-- l)
rest :: [a] -> [a]
rest  xs = tail xs

rest' xs = drop 1 xs

-- Pattern Matching kommt später, aber dennoch gut die Beispiele mal gesehen zu haben
rest''  (x:xs) = xs

rest''' (_:xs) = xs 
                            [ 1, 2]
rest'''' xs = [xs !! i| i <- [1..length xs - 1]]


-- Beispiel: rest'''' "haa"

--  => rest'''' ['h', 'a', 'a']

--    [['h', 'a', 'a'] !! i | i <- [1..length xs - 1]]
--    [['h', 'a', 'a'] !! i | i <- [1,2]]
--    [xs !! 1 : xs !! 2]
--    ['a', 'b']