-- Aufgabe 8-2 Funktionen höherer Ordnung
--

import Prelude hiding (any, all, map, length, concat, reverse)

-- Wir sind nun endlich bei Funktionen höherer Ordnung angekommen. Diese heißen so, wenn sie
-- Funktionen als Argumente nehmen. Das macht Haskell zu einer Programmiersprache die Funkionen 
-- als sog. first-class citizen behandelt

-- Diese Eigenschaft definiert funktionale Sprachen und gibt einen neuen Blick auf Funktionskomposition
-- (falls sich wer noch an Analysis / Lin-Alg erinnert -  f . g = f ∘ g = f(g(x)))
-- aber dazu später mehr

-- *) Warum nimmt die Funktion any nicht ein 'a', 'Bool' und '[a]'?
--    Wenn ein Typ explizit geklammert ist, ist es eine Funktion. Typen sind in Haskell implizit rechtsgeklammert.

-- #### Ausblick für Typen ###

-- D.h ohne explizite Klammerung würde die Funktion 'any' so aussehen:

-- any :: a -> Bool -> [a] -> Bool

-- any :: (a -> (Bool -> ([a] -> Bool)))

-- any :: (a -> (Bool -> ([a] -> Bool)))
-- 
-- let any1 = any 1 :: (Bool -> ([a] -> Bool))
-- 
-- any 1 True :: ([a] -> Bool)
-- 
-- any 1 True [] :: Bool

-- Wenn wir nun aber die explizite Klammerung um (a -> Bool) machen, kann er nicht automatisch eine Klammer reineinsetzen
-- 
-- any :: ((a -> Bool) -> ([a] -> Bool))
--
-- any odd :: ([a] -> Bool)
--
-- any odd [1,2,3] :: Bool
--
-- Da dies aber bisher (soweit ich weiß) noch nicht ausführlich behandelt wurde, kann man sich merken
-- Klammer im Typ => Funktion

-- #### Ende des Ausblicks ###


-- *) Wie man im folgenden sehr gut sehen kann gebe ich der Funktion, die das erste Argument darstellt,
--    einfach den Namen 'pred', genauso wie ich es mit einem Wert machen würde.

-- a) implementieren sie any :: (a -> Bool) -> [a] -> Bool
--
any :: (a -> Bool) -> [a] -> Bool
any pred []     = False
any pred (x:xs)
    | pred x    = True
    | otherwise = any pred xs

-- Wenn das Prädikat für mindestens ein Element der Liste zutrifft wird True zurückgegeben:

-- λ> any even [1,2,3] => True

-- λ> any odd  [2,4,6] => False

-- λ> any (\x -> x == 'l') "Hallo" => True

-- b) implementieren sie all :: (a -> Bool) -> [a] -> Bool
--
all :: (a -> Bool) -> [a] -> Bool
all pred []     = True
all pred (x:xs)
    | pred x    = all pred xs
    | otherwise = False

-- Wenn alle Element der Liste das Prädikat erfüllen wird True zurückgegeben:

-- λ> all odd [1,3,5] => True

-- λ> all even [1,2,4] => False

-- λ> all (\x -> x == 'l') "Hallo" => False

-- c) implementieren sie map :: (a -> b) -> [a] -> [b]

map :: (a -> b) -> [a] -> [b]
map f l = foldr (\x xs -> f x : xs) [] l

foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]



-- λ> map toUpper "bluf"
-- 
-- => foldr (\x xs -> toUpper x : xs) [] "bluf"
-- => toUpper 'b' : foldr (\x xs -> toUpper x : xs) [] "luf"
-- => toUpper 'b' : toUpper 'l' : foldr (\x xs -> toUpper x : xs) [] "uf"
-- => toUpper 'b' : toUpper 'l' : toUpper 'u' : foldr (\x xs -> toUpper x : xs) [] "f"
-- => toUpper 'b' : toUpper 'l' : toUpper 'u' : toUpper 'f' : foldr (\x xs -> toUpper x : xs) [] ""
-- => toUpper 'b' : toUpper 'l' : toUpper 'u' : toUpper 'f' : []
-- => "BLUF"


-- Die Funktionen foldl, foldr und map:

-- ## map ##
--
-- Das mit die erste Funktion die man kennenlernen sollte, wenn man mit higher-order-functions arbeitet.
-- Sie ersetzt die herkömmliche 'for-Schleife' aus anderen Programmiersprachen, mit der Eigenschaft, dass die
-- Liste auf die angewandt wird die gleiche Länge behält.

-- Sie nimmt eine Funktion und eine Liste und wendet die Funktion auf jedes Element nacheinander an
--
--     Funktion   Startliste
--          |         |
--map :: (a -> b) -> [a] -> [b]

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = f x : myMap f xs

e3 = map (+1) [0..5]  -- map (\x -> x + 1) [0..5]
-- [1,2,3,4,5,6]

-- e3
-- map (+1) [0..5]
-- (0+1) : map (+1) [1..5]
-- (1) : (1+1) :map (+1) [2..5]
-- (1) : (2)   : (2+1) : map (+1) [3..5]
-- (1) : (2)   : (3)   : (3+1) : map (+1) [4,5]
-- (1) : (2)   : (3)   : (4)   : (4+1) : map (+1) [5]
-- (1) : (2)   : (3)   : (4)   : (5)   : (5 + 1) : map (+1) []
-- (1) : (2)   : (3)   : (4)   : (5)   : (6)     : []
-- [1,2,3,4,5,6]

-- ## foldl ##
--
-- foldl, (gesprochen fold-left) ist die generische Schreibweise für endrekursive Funktionen (ist dementsprechend immer endrekursiv)
-- Sie nimmt eine Funktion, ein Startelement und irgendwas der Instanz Foldable, aber in unserem Beispiel belassen wir es bei einer Liste

--                Funktion
--            _______|________
--           /                \
--        acc     Element    acc'    Startelement  Startliste
--           |       |        |         |              |
--           |       |        |         |              |
-- foldl :: (b    -> a     -> b)     -> b          -> [a] -> b
-- foldl    (\acc    e     -> acc - e)  0             [1,2,3]

e1 = foldl (\acc     e     -> acc - e)  0             [1,2,3]
--          \0       1     -> -1     => -1            [2,3]
--          \-1      2     -> -3     => -3            [3]
--          \-3      3     -> -6     => -6            []
   => -6

-- foldl fängt mit dem ersten Element der Liste an und führt die Funktion mit dem derzeitigen Akkumulator aus.
-- Dann geht sie von links nach rechts durch die Liste durch und schiebt das akkumulierte Ergebnis als Argument durch

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- e1
-- foldl (..) 0        [1,2,3]
-- foldl (..) (0-1)    [2,3]
-- foldl (..) (-1 - 2) [3]
-- foldl (..) (-3-3)   []
-- (-6)



-- ## foldr ##
-- 
-- foldr (gesprochen fold-right) ist eine eine Funktion die ähnlich wie foldl durch eine Liste durchläuft,
-- mit einer etwas anderen Typsignatur. Der größte Unterschied ist, dass sie von rechts nach links durchläuft (hence 'right')

--                Funktion
--            _______|________
--           /                \
--       Element     acc     acc'      Startelement  Startliste
--           |       |        |           |              |
--           |       |        |           |              |
-- foldr :: (a    -> b     -> b)       -> b          -> [a] -> b
-- foldr    (\e      acc     -> e - acc)  0             [1,2,3]

e2 = foldr (\e      acc      -> e - acc)  0             [1,2,3]
-- 2

-- Da wir aber immernoch mit unseren Listen arbeiten cheaten wir etwas beim den Durchlauf von rechts nach links.
-- Wir sagen nämlich nur, dass das der Akkumulator das Ergebnis vom nächsten rekursiven Aufruf ist. 
-- Dadurch kriegen wir eine lange Kette von Funktionsaufrufen die dann bis zum letzten Element durchgeht und
-- erst dann anfangen kann zu den Ausdruch auszuwerten.

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc []     = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- e2
-- 1 - (foldr (..) 0 [2,3])
-- 1 - (2 - (foldr (..) 0 [3])
-- 1 - (2 - (3 - foldr (..) 0 [])
-- 1 - (2 - (3 - 0))
-- 1 - (-1)
-- 2

-- Übung:
--
-- Definiere die Funktion 'reverse :: [a] -> [a]', 'length :: [a] -> Int', 'concat :: [[a]] -> [a]' mithilfe von foldl / foldr:

-- Definition der abgefragten Funktionen
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]










-- Mögliche Lösungen (vom SS15 / Blatt 4 / H4_L)

-- mit foldl und ausführlicher Lambda-Funktion
concat' :: [[a]] -> [a]
concat' xs = foldl (\ys y -> ys ++ y) [] xs

-- mit foldl
concat'' :: [[a]] -> [a]
concat'' xs = foldl (++) [] xs

-- mit foldr und ausführlicher Lambda-Funktion
concat''' :: [[a]] -> [a]
concat''' xs = foldr (\x xs -> x ++ xs) [] xs

-- mit foldr
concat'''' :: [[a]] -> [a]
concat'''' xs = foldr (++) [] xs


-- Mögliche Lösungen (vom SS15 / Blatt 5 / H5_L)

length'    :: [a] -> Int
length'    l = foldl (\xs x -> succ xs) 0 l

length''   :: [a] -> Int
length''   l = foldl (\xs _ -> succ xs) 0 l

length'''  :: [a] -> Int
length'''  l = foldl (\xs _ -> xs + 1) 0 l

-- const :: a -> b -> a
-- const x _ = x

length'''' :: [a] -> Int
length'''' l = foldl (\xs x -> const (succ xs) x) 0 l

-- Dies kann man aber etwas kürzer schreiben
length''''' :: [a] -> Int
length''''' l = foldl (const . succ) 0 l

-- nur zur Wiederholung
length''''''' :: [a] -> Int
length''''''' l = sum [1 | _ <- l]

reverse'    :: [a] -> [a]
reverse'    l = foldl (\xs x -> x : xs) [] l

reverse''   :: [a] -> [a]
reverse''   l = foldr (\x xs -> xs ++ [x]) [] l

-- flip vertauscht die Argumente von der Funktion
-- Bsp: flip (/) 2 1 => 0.5
--           (/) 2 1 => 2.0
reverse'''  :: [a] -> [a]
reverse'''  l = foldl (flip (:)) [] l

reverse'''' :: [a] -> [a]
reverse'''' l = foldl (\xs x -> flip (:) xs x) [] l
--                          ->       (:) x xs
--                          ->        x  : xs