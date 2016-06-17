-- Aufgabe 9-1 "Binäre Suchbäume"
--

import Prelude hiding (traverse)
import Data.List (sort, genericLength)

-- Einleitung und Beispiele zu dem Thema sind hier zu finden: https://github.com/promo16/promo-ss16/blob/master/SS2016/wiederholung-folien.hs

-- gegeben.

data BB a = L
          | K a (BB a) (BB a)
     deriving Show

-- Binäre Suchbäume sind folgendermaßen definiert:

-- *) ALLE Werte im linken  Teilbaum von jedem Knoten müssen echt kleiner sein als der Wert des Knotens
-- *) ALLE Werte im rechten Teilbaum von jedem Knoten müssen echt größer  sein als der Wert des Knotens

-- Da Binärbäume von Sets stammen, betrachten wir lediglich Mengen die keine doppelten Elemente haben (daher 'echt' kleiner/größer)

-- a) stellen sie [5,7,12,3,1,9] als binären Suchbaum dar

-- Es war zwar nicht gefragt eine Funktion zu schreiben, aber es dennoch eine gute Übung es mit weniger Rekursion als in Teilaufgabe e) zu machen

toBB :: Ord a => [a] -> BB a
toBB l = go (sort l)
    where go :: Ord a => [a] -> BB a
          go []  = L
          go [x] = K x L L
          go xs  = K elem (go left) (go right)
              where
                  mid   = (ceiling (genericLength xs / 2)) - 1
                  elem  = xs !! mid
                  left  = take  mid      xs
                  right = drop (mid + 1) xs


-- Die Idee ist die gleiche, wie man es per Hand machen würde:

-- 1) Sortiere die Liste
--    => [1,3,5,7,9,12]
-- 2) Nimm das mittlere Element (wenn es keins gibt, nehme ich das rechte) und das ist der Hauptknoten
--    => K 5 ? ?
-- 3) Alles was links von der 5 stand kommt in den linken Teilbaum, alles was rechts stand kommt in den rechten Teilbaum
--    => K 5 [1,3] [7,9,12]
-- 4) Und nun rekursiv für die beiden Listen ab Schritt 2 aufrufen
--    => K 5____________
--      /               \
--    K 1               K 9 _
--   /   \             /     \
--  L    K 3         K 7     K 12
--      /   \       /   \   /    \
--     L     L     L    L  L      L

-- => K 5 (K 1 L (K 3 L L)) (K 9 (K 7 L L) (K 12 L L))

-- b) implementieren sie eine Funktion 'isBinarySearch :: Ord a => BB a -> Bool'. Diese überprüft die Sucheigenschaft des Baums

-- partielle Funktion die für 'L' nicht definiert ist, gibt uns immer das linkste Element eines Baums zurück (was das kleinste sein sollte)
--
minV :: Ord a => BB a -> a
minV (K v L _) = v
minV (K _ l _) = minV l

-- partielle Funktion die für 'L' nicht definiert ist, gibt uns immer das rechteste Element eines Baums zurück (was das größte sein sollte)
--
maxV :: Ord a => BB a -> a
maxV (K v _ L) = v
maxV (K v _ r) = maxV r

-- | wir vergleichen den Knotenwert immer mit dem kleinsten Element und dem größten Element in seinen Sub-Bäumen
--
isBinarySearch :: Ord a => BB a -> Bool
isBinarySearch  L               = True
isBinarySearch (K v L    L    ) = True
isBinarySearch (K v L    right) = v < minV right && isBinarySearch right
isBinarySearch (K v left L    ) = v > maxV left  && isBinarySearch left
isBinarySearch (K v left right) = v > maxV left  && isBinarySearch left
                               && v < minV right && isBinarySearch right

-- Wieso funktioniert diese Art von Überprüfung?

-- Für die binäre Suchbaumeigenschaft müssen wir überprüfen:

--    Für jeden Knoten mit Wert 'v' gilt:

--        v > alle Werte im linken Teilbaum und
--        v < alle Werte im rechten Teilbaum

-- Wenn wir jetzt wirklich gegen alle Werte pro Knoten vergleichen würde, ist es ein unnötiger Mehraufwand.
-- Was ist wenn wir nun aber folgendes überprüfen:

--    Ist v > als der größte Wert im linken Teilbaum?

-- Wenn das stimmt, ist v automatisch auch größer als alle anderen Elemente.

-- Ist v < als der kleinste Wert im rechten Teilbaum?
-- 
-- Wenn das stimmt, ist v automatisch auch kleiner als alle anderen Element.
-- Und dadurch können wir uns den Vergleich mit allen Elementen spraen und überprüfen die binäre Suchbaumeigenschaft.



-- alternative Lösung - wir sammeln die Werte in den Knoten auf und schauen ob die rauskommende Liste soritiert ist:
--
toList :: Ord a => BB a -> [a]
toList L                = []
toList (K v left right) = toList left ++ v : toList right

isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = x < y && isSorted (y:ys)

isBinarySearch' :: Ord a => BB a -> Bool
isBinarySearch' tree = (isSorted . toList) tree

-- c) implementieren sie 'depth :: Num a, Ord a => BB t -> a'. Diese gibt die Tiefe des Baums zurück
--
depth :: (Num a, Ord a) => BB t -> a
depth (K _ left right) = 1 + max (depth left) (depth right)
depth L                = 0

-- d) implementieren sie 'insert :: Ord a => a -> BB a -> BB a'. Diese setzt ein Element an der richtigen Stelle eines Baums ein
--
insert :: Ord a => a -> BB a -> BB a
insert e L                = K e L L
insert e (K v left right)
    | e > v     = K v left            (insert e right)
    | otherwise = K v (insert e left) right


-- e) implementieren sie 'buildTree :: Ord a => [a] -> BB a'. Diese erstellt einen möglichen Suchbaum mithilfe von d)

-- foldr ohne Lambda
--
buildTree :: Ord a => [a] -> BB a
buildTree l = foldr insert L l

-- foldr mit Lambda
--
buildTree' :: Ord a => [a] -> BB a
buildTree' l = foldr (\x xs -> insert x xs) L l

--foldl  mit Lambda
--
buildTree'' :: Ord a => [a] -> BB a
buildTree'' l = foldl (\xs x -> insert x xs) L l

-- foldl ohne Lambda
--
buildTree''' :: Ord a => [a] -> BB a
buildTree''' l = foldl (flip insert) L l

-- per Hand
--
buildTree'''' :: Ord a => [a] -> BB a
buildTree'''' l = go l L
    where go :: Ord a => [a] -> BB a -> BB a
          go []     tree = tree
          go (x:xs) tree = go xs (insert x tree)

-- Fast alle Lösungen werden sehr wahrscheinlich extrem degenerierte Bäume bauen die sich stark unterscheiden,
-- weil die Reihenfolge der 'insert's eine Rolle spielt. Deshalb ist der Algorithmus in a) wichtig zu wissen

-- f) Implementieren sie Eq für die Bäume. Reihenfolge sollte keine Rolle spielen und sie sollten binäre Suchbäume sein:

instance Ord a => Eq (BB a) where

  b1 == b2 = isBinarySearch b1
          && isBinarySearch b2
          && b1List == b2List

      where

          b1List = sort $ toList b1
          b2List = sort $ toList b2

-- Wir sammeln alle Elemente aus dem Baum auf, sortieren sie und vergleichen sie mit der eingebauten Eq Instanz für [a]


-- ###############################################################################
-- | Kleiner Einschub zu der Vorlesung 8 - Grundlegende Funktionen für Binärbäume:
-- 
-- Zuerst die Einleitung unter dem Link oben lesen - hier erkläre ich fix ein paar Fakten die man sich aus den Folien
-- mitnehmen sollte:

-- *) Binäre Suchbäume haben die Eigenschaft die oben besprochen wurde:
--    **) sie haben jedoch immer eine Laufzeit bei der Suche von O(n) und nicht O(log2 n),
--        weil sie eben degeneriert sein können (listenartig siehe Folie 8).
--        Big-O Notation sagt uns den WORST-Case, nicht einen den wir uns wünschen
--
--        In anderen Worten:
--           Wenn wir ein Element in einem binären Suchbaum finden wollen, brauchen wir maximal N Schritte dafür
--    
--
-- *) Binäre AUSBALANCIERTE Suchbäume haben folgende Eigenschaften:
--    **) sie haben unterliegen den gleichen Regeln wie binäre Suchbäume
--    **) beim Einfügen werden eventuell Teilbäume verlagert um uns eine Laufzeit bei der Suche von O(log2 n) zu garantieren

--        In anderen Worten:
--            Wenn wir ein Element in einem binären ausbalancierten Suchbaum finden wollen, brauchen wir maximal log2(N) Schritte dafür


-- ## LEXIKALISCHE ANALYSE ##
-- Damit war gemeint, wenn ihr z.B eine eigene Programmiersprache (oder DSL) erstellt, braucht ihr einen Parser der
-- eure "Grammatik", die ihr erlaubt, überprüft und in einen Datentyp verwandelt. Diese Datenstruktur braucht ihr
-- später um sie zu interpretieren und in Maschinencode zu verwandeln
--
-- Im Allgemeinen ist es eigentlich nur ein Parser für einen Sprache mit bestimmten Regeln

-- ## SYNTAX ANALYSE ##
-- Wenn ihr nun diesen Datentyp habt, müsst ihr überprüfen ob da valide Aktionen drinstehen, z.B.
-- kann man hier überprüfen ob der blöde Programmierer nicht aus Versehen durch 0 teilt. Genau kann man 
-- den Code basierend auf Regeln vereinfachen (wie in 9-2 d))

-- ## Typeable ##
-- Diese Instanz auf Folie 11 erlaubt vordefinierte "Casts" wie man sie aus anderen Programmiersprachen kennt.
-- Haskell kann durch 'deriving (Show, Typeable)' eine einfache Stringdarstellung ableiten die man später in
-- eine Exception 'casten' kann.

-- Dieser Code benutzt eine Eigenheit in Haskell, die sich Language Extentions nennt und erweitert die Sprache
-- um Möglichkeiten die oft von Nutzen sind, aber nicht zum Haskell Standart gehören. Daher kann man hier einfach
--    'instance Exception BBExn'
-- ohne ein 'where'-Teil schreiben. Belassen wir es dabei, dass es eine andere Möglichkeit ist Instanzen zu deriven.


-- ## Komischen Durchläufe ##
--
-- *) Prefix -Durchlauf: Erhält   Information über Klammerung
-- *) Infix  -Durchlauf: Verliert Information über Klammerung
-- *) Postfix-Durchlauf: Erhält   Information über Klammerung

-- Die Durchläufe unterscheiden sich lediglich in der Reihenfolge des Einsammelns der Knoten:
--
-- *) Prefix:  Wert -> Linker Teilbaum -> Rechter Teilbaum
-- *) Infix:   Linker Teilbaum -> Wert -> Rechter Teilbaum
-- *) Postfix: Linker Teilbaum -> Rechter Teilbaum -> Wert 

-- Folie 26. Nun wird eine Funktion gezeigt, die eine anonyme Funktion als Argument nimmt wodurch sich die jeweiligen Durchläufe
-- abstrahieren lassen. Machen wir sie etwas deskriptiver:

data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving Show

-- foldr :: acc -> (a -> acc -> acc) -> [a] -> acc

-- |       Akkumulator      Funktion            Baum
--           |       __________|_________         |
--           |      /                    \        |
traverse :: acc -> (a -> acc -> acc -> acc) -> Tree a -> acc
traverse acc f Empty                   = acc
traverse acc f (Node value left right) = f value (traverse acc f left) (traverse acc f right)

-- Unsere Funktion kriegt einen 'Plan' wie sie mit dem Wert am Knoten un jeweils Werten vom linken und rechten Teilbaum anstellen soll.
--
--Wert am Knoten  Linker Teilbaum
--  |               |
--  |        _______|   Rechter Teilbaum
--  |       /      _________|
--  |      /      /
-- (a -> acc -> acc -> acc)
--                      |
--                    Rückgabewert

-- Die Durchläufe abstrahiert
--
prefixTraverse :: Tree a -> [a]
prefixTraverse  tree = traverse [] (\value left right -> value : left ++        right           ) tree

infixTraverse :: Tree a -> [a]
infixTraverse   tree = traverse [] (\value left right ->         left ++ value :right           ) tree

postfixTraverse :: Tree a -> [a]
postfixTraverse tree = traverse [] (\value left right ->         left ++        right ++ [value]) tree

-- Ein Beispieldurchlauf für prefixTraverse:

-- Node 1 (Node 2 Empty Empty)
--        (Node 3 Empty
--                     (Node 4 Empty Empty))

--              1
--             / \
--            2   3
--                 \
--                  4

--    prefixTraverse Node 1 (Node 2 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))
-- => traverse [] (\value left right -> value : left ++ right) Node 1 (Node 2 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))
--    ------------------------------------------------------------------------------------------------------------------------

-- value = 1
-- left  = traverse [] (\value left right -> value : left ++ right) (Node 2 Empty Empty)
-- right = traverse [] (\value left right -> value : left ++ right) (Node 3 Empty (Node 4 Empty Empty))

-- => 1 : traverse [] (\value left right -> value : left ++ right) (Node 2 Empty Empty) ++ traverse [] (\value left right -> value : left ++ right) (Node 3 Empty (Node 4 Empty Empty))
--        ----------------------------------------------------------------------------
-- value = 2
-- left  = Empty    (traverse [] f Empty = [])
-- right = Empty    (traverse [] f Empty = [])

-- => 1 : 2 : [] ++ [] ++ traverse [] (\value left right -> value : left ++ right) (Node 3 Empty (Node 4 Empty Empty))
--                        --------------------------------------------------------------------------------------------

-- value = 3
-- left  = Empty    (traverse [] f Empty = [])
-- right = Node 4 Empty Empty

-- => 1 : 2 : [] ++ [] ++ 3 : [] ++ traverse [] (\value left right -> value : left ++ right) (Node 4 Empty Empty)
--                                  -----------------------------------------------------------------------------

-- value = 4
-- left  = Empty    (traverse [] f Empty = [])
-- right = Empty    (traverse [] f Empty = [])

-- => 1 : 2 : [] ++ [] ++ 3 : [] ++ 4 : [] ++ []

-- => [1,2] ++ [3] ++ [4]

-- => [1,2,3,4]

-- Wer sich die Beispielauswertung von 'foldr' in 8-2 anschaut wird ein paar Ähnlichnkeiten feststellen.
-- Wir schieben den Akkumulator immer mit und werten praktisch "von hinten" aus.

-- Der einzigste Unterschied ist dass die Funktion die wir akzeptieren 3 Argumente nimmt und nicht 2, wie bei foldr.
-- Das kommt nur daher, weil wir eben immer zwei Teilbäume haben und nicht immer ein Nachfolgeelement wie bei der Liste.

-- Die anderen Beispiele die er macht, nutzen den sog. point-free style auf den ich nicht weiter eingehen werde. Hier sind die äquivalenten 
-- Definitionen:

anzahlKnoten :: Tree a -> Int
anzahlKnoten tree = traverse 0 (\value left right -> 1 + left + right) tree

-- | Ich bin mir nicht ganz sicher warum hier in der Folie durch 2 geteilt wurde, eigentlich sollte diese Definition stimmen
--
anzahlBlätter :: Tree a -> Int
anzahlBlätter tree = traverse 1 (\value left right -> left + right) tree

baumTiefe :: Tree a -> Int
baumTiefe tree = traverse 0 (\value left right -> 1 + max left right) tree

isIn :: Eq a => a -> Tree a -> Bool
isIn element tree = traverse False (\value left right -> element == value || left || right) tree

-- Der Breitendurchlauf ist in den Wiederholungsfolien erklärt.