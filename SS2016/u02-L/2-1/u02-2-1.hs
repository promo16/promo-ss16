-- Blatt 2, Aufgabe 2-1

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
a = compare 2 3  -- LT
-- F: Ordering
-- G: Ordering

-- Erklärung: Die Funktion hat compare hat folgende Typsignatur:

--     Ord a => a -> a -> Ordering
--
-- das heißt sie nimmt zwei Argumente deren Typ in der Klasse Ordering definiert ist
-- und bildet sie auf einen neuen Typ "Ordering" ab.

-- Dieser ist vergleichbar hat 3 mögliche Werte die er annehmen kann:
--
-- LT ^= Lesser than
-- EQ ^= Equal
-- GT ^= Greater than

-- Man kann es vielleicht mit dem Compareable Interface in Java vergleichen.
-- In diesem Fall vergleichen wir zwei Ints und da wird geschaut ob die erste Zahl
-- größer, kleiner, oder gleich groß ist wie die zweite.

-- Beispiele:

-- compare 3 3 => EQ
-- compare 2 3 => LT
-- compare 3 2 => GT

-- compare 'a' 'b' => LT (es wird der ASCII Wert genommen)
--                 => compare 97 98

-- compare "hi" "du" => GT (bei Listen wird zuerst das erste Element verglichen, dann das zweite usw.)
--                   => compare 'h' 'd'
--                   => compare 104 100 => GT

-- b)
b = odd 3 -- True
-- F: Bool
-- G: Bool

-- Erklärung: Überprüft ob eine Zahl ungerade ist oder nicht

-- c)
c = even 3 -- False
-- F: Bool
-- G: Bool

-- Erklärung: Überprüft ob eine Zahl gerade ist oder nicht

-- d)
d = drop 3 [1,2,3,4,5] -- [4,5]
-- F: [Integer]
-- G: Num a => [a]

-- Erklärung: Die Funktion drop hat die folgende Typsignatur:

--     Int -> [a] -> [a]

-- das heißt sie nimmt ein Zahl und eine Liste und gibt uns widerrum eine Liste des
-- selben Typs wieder. In diesem Fall wirft sie die ersten n Elemente weg, wobei n
-- die Zahl ist.

-- Beispiel:

-- drop 1  [1,2,3] => [2,3]
-- drop 1  "Hallo" => "allo"
-- drop 10 "Hallo" => ""
-- drop 2  ['a', 'b', 'c'] => ['c']

-- Bemerkung: Es ist nicht (Enum a, Num a) => [a]. Das passiert wenn man im GHCi 'drop 3 [1..5]' eingibt.

-- e)
e = elem 3 [1,2,3,4,5] -- True
-- F: Bool
-- G: Bool

-- Erklärung: Die Funktion elem hat die folgende Typsignatur:

--     (Eq a, Foldable t) => a -> t a -> Bool
--
-- in unserem Fall können wir wieder Foldable weglassen und so tun als wäre es eine Liste:

--     (Eq a) => a -> [a] -> Bool
--
-- das heißt die Funktion nimmt ein Element und eine Liste vom selben Typ und 
-- gibt uns ein Bool Wert zurück. Sie überprüft ob dieses Element in der Liste vorkommt.

-- Die Eq Instanz für diesen Typ muss bestimmt sein, weil die Elemente vergleichbar sein müssen.
-- Sie sorgt dafür dass (==) definiert ist.

-- f)
f = sum [1,2,3,4,5] -- 15
-- F: Integer
-- G: Num a => a

-- Erklärung: Die Funktion sum summiert alle Elemente auf.

-- Bemerkung: Es ist nicht (Enum a, Num a) => a. Das passiert wenn man im GHCi 'sum [1..5]' eingibt.


-- g)
g = reverse "Lag er im Kajak, mir egal" -- "lage rim ,kajaK mi re gaL"
-- F: [Char]
-- G: [Char]

-- Erklärung: Die Funktion reverse dreht die Liste um.

-- Beispiel:

-- reverse [1,2,3,4]  => [4,3,2,1]
-- reverse [1]        => [1]
-- reverse ['a', 'b'] => ['b', 'a']

-- h)
h = words "Lag er im Kajak, mir egal" -- ["Lag","er","im","Kajak,","mir","egal"]
-- F: [[Char]] ([String])
-- G: [[Char]] ([String])

-- Erklärung: Die Funktion words spaltet ein String in kleinere Strings, wenn sie
--      durch ein Leerzeichen abgegrenzt waren. Ein String ist eine Liste von Chars,
--      wenn also ein String in kleinere Strings aufgesplatet wird, sind es nun eine Liste von String.
--      Das ist äquivalent zueinander [ [Char] ] == [ String ]

-- i)
i = unlines ["Lag","er","im","Kajak,","mir","egal"] -- "Lag\ner\nim\nKajak,\nmir\negal\n"
-- F: [Char]
-- G: [Char]

-- Erklärung: Die Funktion unlines fügt zwischen jeden String ein '\n'-Char dazwischen.


