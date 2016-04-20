module Main where


import Data.Maybe
import Data.Char

-- | Ein paar sinnvolle Beispiele und Erklärung zum Code

-- | falls die Liste leer ist
a1 = listToMaybe []
--  => Nothing

-- | falls sie nicht leer ist, gibt das erste Element zurück
a1' = listToMaybe [1,2,3]
-- => Just 1


-- | read kann Strings (für unseren Fall zu Ints) parsen,
--   die ausschließlich aus Zahlen bestehen

a3 = read "12" :: Int
-- => 12

a3' = read "12sadd" :: Int
-- => *** Exception: Prelude.read: no parse


-- | reads kann Strings zu Ints parsen, die auch einen Rest beinhalten
a4 = reads "12fsdfd21" :: [(Int, String)]
-- => [(12, "fsdfd21")]

-- a3 = reads "12"
-- a3 = read  "12"
-- => gibt beides einen Fehler aus, da wir erst durch Typisierung read(s) sagen
--    welchen Typ wir rauskriegen wollen


-- | elem überprüft ob das Element in der Liste drin ist und gibt ein Bool zurück
a5 = 1 `elem` [1..6]
-- => True

a5' = elem 'c' "Hello World!"
-- => False


-- | span nimmt solange Elemente bis das Predikat wahr ist und gibt dann das Tupel
--   aus der entnommenen Liste und dem Rest zurück
a6 = span (/= 4) [1..9]
-- => ([1,2,3],[4,5,6,7,8,9])

-- | ord ist der ASCII Wert von einem Char
--   ord ' ' = 32
a6' = (\x -> ord x > 60) `span` "Go to hayoo.fh-wedel.de to search functions!"
-- => ("Got"," to hayoo.fh-wedel.de to search functions!")


-- | null überprüft ob die Liste leer ist und gibt ein Bool zurück
a7 = null []
-- => True

a7' = null "Blub"
-- => False



-- | A7-3

gaa :: (Read a, Bounded d, Eq a) => a -> String -> d -> d

--gaa :: (Read a, Eq a, Bounded b) => a -> String -> b -> b
gaa xy z vw = if (read z == xy) then minBound else vw


-- | Beispiel für Bounded

data Day = Mon | Tue | Thu | Wed | Fri | Sat | Sun
  deriving (Bounded, Show)

-- | Bounded definiert die obere und untere Grenze von etwas!
--   Es ist aber nicht geordnet (nicht wie Enum)

minDay :: Day
minDay = minBound

maxDay :: Day
maxDay = maxBound


-- guu :: a -> b -> c

-- das zweite Argument ist eine Liste

-- => guu :: a -> [b] -> c

-- auf das erste Argument wird 'show' angewendet

-- => guu :: Show a => a -> [b] -> String

-- Was wird in eine Liste gepackt ein String ergeben? -> ein Char

-- => guu :: Show a => a -> [Char] -> String

guu :: Show a => a -> [Char] -> String
guu _ (h1:h2:t) = [h2]
guu x _         = show x

-- axx :: a -> (b, c) -> d

-- w wird mit einem '||' verknüpft - es muss ein Bool sein

-- axx :: a -> (b, Bool) -> d

-- auf v wir succ angewendet -> das kann nur sein, wenn es eine Typinstanz von 'Enum' ist

-- axx :: Enum b => a -> (b, Bool) -> d

-- minBound ist in diesem Fall ein Bool

-- => minBound :: Bool ==^ False

-- v und u müssen vom selben Typ sein, da der 'then' und 'else' Fall den selben Typen rausgeben

-- axx :: Enum b => b -> (b, Bool) -> d

-- wir geben ja v oder u zurück, damit gibt die Funktion auch den Typ davon zurück

axx :: Enum b => b -> (b, Bool) -> b
axx u (v, w) = if minBound || w then succ v else u


-- foo :: a -> b -> c -> d -> e -> f

-- das dritte Argument ist eine Liste

-- foo :: a -> b -> [c] -> d -> e -> f

-- auf das erste, dritte und vierte Argument wird 'show' angewendet

-- foo :: (Show a, Show c, Show d) => a -> b -> [c] -> d -> e -> f

-- auf das zweite Argument wird 'read' angewendet -> es ist ein String

-- foo :: (Show a, Show c, Show d) => a -> String -> [c] -> d -> e -> f

-- das Ergebnis von der Funktion ist immer ein String, da show :: a -> String
-- und (++) dadurch auch für Strings funktioniert

-- foo :: (Show a, Show c, Show d) => a -> String -> [c] -> d -> e -> String

-- da wir ein String rauskriegen und auf das vierte Argument (++) angewendet wird, muss es ein String sein

-- foo :: (Show a, Show c) => a -> String -> [c] -> String -> e -> String

-- da das erste Argument mit dem dritten Argument vergleichen wird, muss es eine instanz von Eq und dadurch auch der selbe Typ sein

foo :: (Eq a, Show a) => a -> String -> [a] -> String -> e -> String
foo a b []    e f = show a
foo a b (c:d) e f
  | a == c, read b = e ++ e
  | a /= c         = show d
  | otherwise      = show e



-- Nachtrag zu dieser Teilaufgabe:

-- Wenn man diesen Typ auskommentiert und sich ihn von GHCi inferieren lässt, meint er, dass er so aussieht:

-- bar :: Ord a => a -> a -> a -> a -> Bool

-- Da es aber eine polymorphe rekursive Funktion ist, schafft es der Compiler leider nicht, den allgemeinsten Typ zu finden.

-- Erklärung:

-- Wir nehmen 4 Argumente entgegen und haben ein Rückgabewert

-- bar :: a -> b -> c -> d -> e

-- Wir sehen, dass b und c vergleichen werden, das heißt dass beide vom gleichen Typ sind und eine Instanz in der Typklasse Ord besitzen

-- bar :: (Ord b) => a -> b -> b -> d -> e

-- Wir sehen auch, dass a mit d gleichgesetzt wird und darauf kein rekursiver Aufruf folgt! Daraus kann man herleiten, dass unser Rückgabewert
-- ein Bool ist, sowie, dass a und d vom selben Typ sind und auch eine Instanz in der Typklasse Eq besitzen

-- bar :: (Ord b, Eq a) => a -> b -> b -> a -> Bool

-- Schauen wir den rekursiven Funktionsaufruf genauer an - wir vertauschen die Argumente b und a, sowie d und c miteinander!

-- => das heißt, dass wir im nächsten Schritt unser ursprüngliches a und d mit (>=) vergleichen!
-- => daraus folgt, dass sie auch eine Instanz in der Typklasse Ord besitzen!

-- bar :: (Ord b, Ord a) => a -> b -> b -> a -> Bool

-- Wieso ist nun 'Eq' verschwunden? Weil Ord eine Unterklasse von Eq ist <=> Alles was in Ord ist, ist auch automatisch in Eq!

bar :: (Ord a, Ord b) => a -> b -> b -> a -> Bool
bar a b c d
  | b >= c = bar b a d c
  | otherwise = a == d




