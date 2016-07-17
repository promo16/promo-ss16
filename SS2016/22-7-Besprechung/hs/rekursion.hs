import Prelude hiding (take, repeat)

-- Aufgaben zu Rekursion
--
-- Da ich wenig Ideen habe, nehme ich einfach ein paar Funktionen aus Data.List.
--
-- Definieren sie folgende Funktionen:

-- a) intersperse :: a -> [a] -> [a] 
--
--  Diese Funktion nimmt ein Separator, eine Liste und packt zwischen jedes Element den Seperator.
--  Es kommt kein Separator vor das erste oder hinter das letzte Element der Liste.
-- 
-- λ> intersperse ',' "hallo"
-- "h,a,l,l,o"
-- 
-- λ> intersperse 0 [1,2,3]
-- [1,0,2,0,3]
--
-- λ> intersperse 0 [1]
-- [1]


-- Lösung:

intersperse :: a -> [a] -> [a]
intersperse x []  = []
intersperse x [y] = [y]                           -- Der Fall ist wichtig, da wir kein 'x' hinter dem letzten Element haben wollen
intersperse x (y:ys) = y : x : intersperse x ys


-- b) at :: [a] -> Int -> a
-- 
-- Diese Funktion gibt mir das Element am jweiligen Index der Liste zurück.
-- Fehlerbehandlung sind nicht nötig:
--   *) KEIN Überprüfen ob der Index valide ist (nicht-negativ, oder zu groß)
-- 
-- (!!) darf nicht benutzt werden.

-- λ> at [1,2,3] 0
-- 1

-- λ> at "hallo" 4
-- 'o'

at :: [a] -> Int -> a
at (x:_)  0 = x
at (x:xs) n = at xs (n-1)


-- c) take :: Int -> [a] -> [a]
--
-- Diese Funktion nimmt eine Zahl 'n' und eine Liste 'xs' und gibt den Prefix der Liste mit Länge 'n' zurück
-- oder die Liste selbst, falls 'n' > length 'xs'
--
-- Fehlerbehandlung sind nicht nötig:
--   *) KEIN Überprüfen ob 'x' negativ ist
-- 
--
-- λ> take 3 [1,2,3,4,5]
-- [1,2,3]
--
-- λ> take 10 [1,2,3]
-- [1,2,3]
--
-- λ> take 0 [1,2,3]
-- []

take :: Int -> [a] -> [a]
take 0    xs  = []
take n    []  = []
take n (x:xs) = x : take (n-1) xs


-- d) repeat :: a -> [a]

-- Diese Funktion nimmt ein Argument 'x' und erstellt eine unendlich große Liste nur 'x' als Inhalt
--
-- Pseudobeispiel:
--    repeat 1 => [1,1,1,1,1...

-- λ> take 10 (repeat 1)
-- [1,1,1,1,1,1,1,1,1,1]

-- λ> take 5 (repeat 'a')
-- "aaaaa"

-- λ> take 2 $ repeat []
-- [[],[]]

repeat :: a -> [a]
repeat x = x : repeat x

-- e) divBy3 :: [Int] -> [Int]
--
--   Definieren sie mithilfe von List-Comprehentions eine Funktion die alle durch 3 teilbaren Zahlen
--   zurückgibt.
--
-- λ> divBy3 [1..10]
-- [3,6,9]
--
-- λ> divBy3 [(-1),(-2)..(-10)]
-- [-3,-6,-9]

divBy3 :: [Int] -> [Int]
divBy3 list = [x | x <- list, mod x 3 == 0]