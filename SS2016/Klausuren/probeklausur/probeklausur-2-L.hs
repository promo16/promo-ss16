-- | Beispiellösung zur Probeklausur 2 vom 4.7
--


-- | Aufgabe 1:
--
-- gegeben:

data BB a = L
          | K a (BB a) (BB a)

b = K 1 (K 2 (K 3 L L) (K 4 L L))  (K 5 (K 6 L L) (K 7 L L))


tief :: a -> (b -> a -> a -> a) -> BB b -> a
tief acc f  L               = acc
tief acc f (K w left right) = f w (tief acc f left) (tief acc f right)

-- Hier kann man gut das Pattern erkennen, dass es sich lediglich um ein fold-left handelt (ich hab den Akkumulator hinter die Hilfsfunktion geschoben)!
-- Das zusätzliche Argument in der ersten Funktion kommt daher, dass Knoten zwei Kinder haben - links und rechts. Bei einer Liste haben
-- wir aber nur einen Nachfolger.

-- tief  :: (b -> a -> a -> a) -> a -> BB b -> a
-- foldl :: (b -> a -> a)      -> a -> [b] -> a

-- BB steht für Binärbaum
-- L steht für Leer
-- K steht für Knoten

-- Ergänzen sie:

anzahlKnoten :: BB a -> Int
anzahlKnoten baum = tief 0     (\w links rechts -> undefined) baum

baumTiefe :: BB a -> Int
baumTiefe    baum = tief 0     (\w links rechts -> undefined) baum

istIn :: Eq a => a -> BB a -> Bool
istIn  wert baum = tief False (\w links rechts -> undefined) baum


-- | Lösung:
--
anzahlKnoten' :: BB a -> Int
anzahlKnoten' baum = tief 0     (\w links rechts -> 1 + links + rechts) baum
--                                |   |     |       \________________/
--                               'a' Int   Int               |
--                                                          Int

baumTiefe' :: BB a -> Int
baumTiefe'    baum = tief 0     (\w links rechts -> 1 + max links rechts) baum
--                                |   |     |       \___________________/
--                               'a' Int   Int               |
--                                                          Int

istIn' :: Eq a => a -> BB a -> Bool
istIn'  wert baum = tief False (\w links rechts -> wert == w || links || rechts) baum
--                               |    |      |     \__________________________/
--                              'a'  Bool   Bool             |
--                                                          Bool

prefix :: Eq a => BB a -> [a]
prefix baum = tief [] (\w links rechts -> [w] ++ links ++ rechts) baum

infixB :: Eq a => BB a -> [a]
infixB baum = tief [] (\w links rechts -> links ++ [w] ++ rechts) baum

postfix :: Eq a => BB a -> [a]
postfix baum = tief [] (\w links rechts -> links ++ rechts ++ [w]) baum


-- Erklärung:
--
-- Die Schwierigkeit dieser Aufgabe bestand darin zu verstehen,
--    *) dass 'w' vom Typ 'a' und
--    *) 'links' und 'rechts' vom jeweiligen Rückgabewert der Funktion waren ('Int', 'Bool'),
-- weil die Funktion 'tief' so definiert war.
--
-- Hierzu haben wir aber bereits viel gemacht (tief ist in dem Link 'traverse' genannt und wird im Einzelnen durchgegangen):
--      https://github.com/promo16/promo-ss16/blob/master/SS2016/u09-L/9-1.hs
--

-- | Aufgabe 2:
--
-- Zwei Arten von Dokumenten werden erfasst:
--
-- *) Artikel:
--    **) einem oder mehreren Autoren
--    **) einem Titel

-- *) Buch:
--    **) null, einem oder mehreren Autoren
--    **) einem Titel

-- Es wird angenommen dass Autoren und Titel beliebige Zeichenketten sind.

-- 1) Definieren sie einen rekursiven Typ 'Autoren' mit Konstruktoren:

--  *) EA (für genau Einen Autor)
--  *) MA (für Mehrere Autoren)

data Autoren  = EA  String
              | MA  String  Autoren

-- (alternative Lösung)
data Autoren' = EA' String
              | MA' String [Autoren']

data Autoren'' = MA'' String Autoren''
               | KeinAutor

-- 2) Geben sie einen Wert des von Ihnen definierten Typs Autoren für die folgenden Autoren

a2  = MA  "a1" (MA  "a2" (EA  "a3"))
a2' = MA' "a1" [EA' "a2", EA' "a3"]


-- 3) Definieren sie

--  a) einen Typ 'Artikel' mit Konstruktor 'A' für Artikel (beachte dass Artikel mid. 1 Autor haben müssen)
--  b) einen Typ 'Buch' mit Konstruktor 'B' für Bücher     (beachte dass Bücher auch keinen Autor haben müssen)
--  c) eine Funktion 'aTitel :: Artikel -> String', die den Titel eines Artikels zurückgibt
--  d) eine Funktion 'bTitel :: Buch -> String', die den Titel eines Buches zurückgibt

-- a)

data Artikel  = A String Autoren

-- (alternative Lösung, die c) automatisch löst, weil aTitel = artikelTitel)
data Artikel' = A' { artikelTitel :: String, artikelAutoren :: Autoren }


-- b)

data Buch    = B String (Maybe Autoren)

data Buch'   = B' String [Autoren]

-- (alternative Lösung, die d) automatisch löst, weil bTitel = buchTitel)
data Buch''  = B'' { buchTitel :: String, buchAutoren :: Maybe Autoren }

-- Warum ist 'data Buch = B [String] String' semi-optimal (nicht indiomatisches Haskell)?

-- tl;dr - Doppelt gemoppelte Liste ist unnötig, Maybe eignet sich viel besser dafür
--
-- Weil wir vor ungefähr 15 Zeilen Autoren definiert haben und nun fassen wir sie als normale Strings auf,
-- was nicht konsistent ist. Vorallem wenn wir sie eine rekursiv definierte Liste darstellen,
-- aber nun packen wir nochmal eine Liste drumrum, die unnötig ist.

-- Eine viel bessere Lösung wäre das als 'Maybe Autoren' darzustellen, dass den Fall von 'kein Autor' abfängt, und 'Autoren'
-- dafür verantwortlich sind einen oder mehr Autoren zu definieren. 


-- c)

aTitel :: Artikel -> String
aTitel (A titel _) = titel

-- In der Musterlösung fehlen hier Klammern um das Argument

-- d)

bTitel :: Buch -> String
bTitel (B titel _) = titel

-- In der Musterlösung fehlen hier Klammern um das Argument

-- 4)

-- Sei folgende Typklasse definiert:

class Dok a where

    dokTitel :: a -> String

-- Ergänzen sie die folgenden Definitionen von Artikel und Buch als Instanzen der Typklasse Dok:

-- Zum einen wurde hier die Typvariable 'a' vergessen. In der Klassendefinition sowie in der Funktion.

-- Hier stellt sich die Frage was man machen soll? Man kann nur den Titel zurückgeben, nur die Autoren konkatiniert,
-- beides zusammen oder auch nichts davon?

-- Anscheinend war hier der Titel gefragt und dadurch können wir die Funktionen von der 3) benutzen.

instance Dok Artikel where

    dokTitel artikel = aTitel artikel

instance Dok Buch where

    dokTitel buch = bTitel buch

-- Wer mit der Musterlösung vergleicht, merkt dass dort die Argumente weggelassen wurden, weil wieder Point-free Syntax angewendet wurde,
-- die ihr nicht behandelt hat. Die obere Lösung ist genauso korrekt.



-- | Aufgabe 3:

-- 1) Geben sie die Definition eines neuen Typs Zahl a, wobei a eine Typvariable ist, mit einem einzigen Konstruktor Z der Stelligkeit 1

-- Was ist nun Stelligkeit? Gemeint ist damit wohl, dass der Konstruktor Z genau ein Argument nimmt. Aber was für einen?
-- Keiner hindert mich daran sowas zu schreiben "data Zahl a = Z Int"...

-- Es war wohl gedacht, dass 'Zahl' genau diesen Typ 'a' als erste Argument nimmt.

data Zahl a = Z a


-- 2) Ergänzen sie die folgenden Funktionsdefinition:

-- plus :: ..... a => Zahl a ......
-- plus .....   ..... = Z (x1 + x2)

-- Schwer zu verstehen, weil nur eines von zwei Argumenten gegeben wurde. Das einzigste was man hier erkennen kann ist dass 'a'
-- von irgendeiner Typklasse eingeschränkt wird, dass 'x1' und 'x2' vom gleichen Typ sind und dass wir eine Zahl zurückgeben.

-- plus :: (... a, Num b) => Zahl a -> .... -> Zahl b
-- plus .....   ..... = Z (x1 + x2)

-- Nun können wir eigentlich alles mögliche erfinden:

plus1 :: (Num a) => Zahl a -> (Int, Int) -> Zahl Int
plus1 (Z _) (x1, x2) = Z (x1 + x2)

plus2 :: (Bounded a) => Zahl a -> [Double] -> Zahl Double
plus2 (Z _) [x1, x2] = Z (x1 + x2)

plus3 :: (Enum a) => Zahl a -> (Word, String, Word, Double, Integer) -> Zahl Word
plus3 (Z _) (x1,_,x2,_,_) = Z (x1 + x2)

plus4 :: (Num b, Ord a) => Zahl a -> [(b, String)] -> Zahl b
plus4 (Z _) ((x1,_):(x2,_):_) = Z (x1 + x2)

-- Gedacht war natürlich dass man zwei 'Zahl' Typen miteinander addiert:

plus :: (Num a) => Zahl a -> Zahl a -> Zahl a
plus (Z x1) (Z x2) = Z (x1 + x2)


-- 3) Ergänzen sie die folgenden Monoid-Definition so, dass Zahl a (für a ein Typ Integral) ein Monoid für die
--    Addition ist:

-- instance ..... => Monoid (Zahl a) where

--     mempty = ...

--     mappend z1 z2 = ...

-- Hier können wir dank einer vordefinierten Klasse nicht soviel Schmarn machen. Erinnerung an die Regeln die ein Monoid
-- erfüllen muss:

--  *) Linksneutral:   mempty `mappend` x      = x
--  *) Rechtsneutral:  x      `mappend` mempty = x
--  *) Assoziativität: (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- Addition erfüllt das aber für uns, deswegen ist das praktisch geschenkt.
--
-- Die Integral Einschränkung füllt man vor dem (=>) ein.

instance (Integral a) => Monoid (Zahl a) where

    mempty = Z 0

    mappend z1 z2 = plus z1 z2

-- Wieso kein (Num a)? Weil Integral automatisch auch von Num eingeschränkt ist.
-- http://dev.stephendiehl.com/hask/img/numerics.png


-- | Aufgabe 4:
--
-- gegeben:

anwenden :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
anwenden op sz1 sz2 = do
    z1 <- sz1
    z2 <- sz2
    return (op z1 z2)

-- 1) Erstellen sie eine Funktion 'plus' die zwei 'Maybe Int's miteinander addiert, mithilfe von 'anwenden' und (+)
  

-- Erklärung:
--
--  Zuerst mal den Typ hinschreiben (der leider auch nicht offensichtlich am Ende ein Maybe Int zurückgibt...)
--
--  Die Musterlösung benutzt wieder Pointfree-Style

plus' :: Maybe Int -> Maybe Int -> Maybe Int
plus' mx my = anwenden (+) mx my

-- 2) Definieren sie ohne anwenden einzusetzen eine zweistellige Funktion division mit Typ 'Maybe Int -> Maybe Int -> Maybe Int',
--    sodass eine Divison mit dem Null-Element von Maybe Int den passenden Wert vom Typ 'Maybe Int' zurückgibt.
--
--    Was ist der 'passende Wert'? Wahrscheinlich der Nothing-Konstruktor...
--
--    Erklärung:
--
--       Hier gibt es unzähliche Möglichkeiten das zu schreiben.

divison :: Maybe Int -> Maybe Int -> Maybe Int
divison mx (Just 0) = Nothing
divison mx my = do
    x <- mx
    y <- my
    return (x `div` y)

divison' :: Maybe Int -> Maybe Int -> Maybe Int
divison' mx my = do
    x <- mx
    y <- my
    if y == 0
        then Nothing
        else Just (x `div` y)

divison'' :: Maybe Int -> Maybe Int -> Maybe Int
divison'' mx (Just 0) = Nothing
divison'' mx my       = fmap div mx <*> my

division''' :: Maybe Int -> Maybe Int -> Maybe Int
division''' (Just x) (Just y)
    | y == 0    = Nothing
    | otherwise = Just (x `div` y)
division''' _ _ = Nothing