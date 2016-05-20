-- | Foliensatz 4: fib4.hs
-- ######################################################################################################

import Data.Maybe (fromJust)

main :: IO ()
main = print $ fib 30


fib :: Integer -> Integer
fib n = fst (hfib n [(1,1), (0,0)])
    where
        hfib :: Integer -> [(Integer, Integer)] -> (Integer, [(Integer, Integer)])
        hfib n memo
            | n <= fst (head memo) = (myLookup n memo, memo)   -- (I)
        hfib n memo = let (fibn1, memo1) = hfib (n-1) memo     -- (II)
                          (fibn2, memo2) = hfib (n-2) memo1    -- |
                          fibn           = fibn1 + fibn2       -- |
                      in (fibn, (n, fibn) : memo2)             -- |

myLookup :: Integer -> [(Integer, Integer)] -> Integer
myLookup n ((m, fibn) : memo) = if n == m
                                    then fibn
                                    else myLookup n memo

-- | Also zum einen kann man die Funktion myLookup besser schreiben:

-- myLookup :: Eq a => a -> [(a,b)] -> b
-- myLookup k list = fromJust $ lookup k list

-- so ist sie aber immernoch partiell, weil der Fall der leeren Liste nicht abgefangen ist

-- | (I) sehr schlechter Stil

-- hfib n memo@((m,_):ms)
--    | n <= m = (myLookup n memo, memo)

-- | Hier wird versucht per Hand Memoisation durchzuführen (so wie man es in Java machen würde)
--
--   Die Idee ist folgende - wenn ich einmal z.B fib (6) ausgerechnet habe, will man sich das unter dem Index
--   6 in einer Liste abspeichern, Wenn man dann das Ergebnis nochmal braucht, schaut man nach ob unter diesem Index
--   bereits ein Ergebnis liegt (I) . Falls nein, dann berechnet man es sich und speichert es ab (II)

--   Was hier aber noch gemacht wird ist etwas unüblich. Die Liste wir von vorne aufgefüllt und es wird ausgenutzt
--   dass wir uns bei der Berechnung von Fibonacci Zahlen immer genau auf das vorherige Ergebnis (also 'n-1') referenzieren.
--   Deswegen wird bei (I) auch nur überprüft ob bereits das erste Element dieser Liste der Zahl an der n-ten Stelle entspricht.

--   Das Problem bei so einem Ansatz ist dass es nie wieder verwendbar ist (auch wenn das anhängen der Elemente an die Liste konstant ist)
--   Für leicht andere Probleme stößt man auf Probleme dass man evtl unnötige Berechnungen macht, weil man von vorne durchgeht und alles
--   auswertet was nicht bekannt ist, obwohl man nur das vorletzte Element braucht.



-- | Eine andere Implementierung

--   Ein viel einfacherer Ansatz der auf mehrfach rekursive Funktionen anwendbar ist und Kreuzreferenzen effizient behandelt ist der folgende.
--   Wir können ausnutzen, dass eine Liste indexiert ist und wir damit auf Elemente zugreifen können.
--   
--   Was wir lediglich erstellen müssen ist eine Liste und eine Funktion die mir aus GENAU dieser Liste das 'i'-te Element zurückgibt.
--   
--    * 'resultList' ist eine unendliche Liste auf die die Funktion 'toFib' gemappt wurde.
--    * 'getMe' ist die Funktion die sich immer auf die Liste 'resultList' referenziert und mir das Element zurückgibt
--   
--   Weil die Liste nur einmal definiert ist und 'getMe' auch nur innerhalb von 'foo' darauf zugreift wird diese nur einmal berechnet
--
--    * toFib ist die Funktion die mir die Zahlen berechnet. Wenn die Zahl aus vorherigen Zahlen besteht, schaut sie in der bereits berechneten
--      Liste genau an den gefragten Stellen nach, die sich dann jeweils aktualisieren (w.g lazy evaluation) und den Wert bereitstellen.
--
--   Wird nun ein neuer Aufruf auf den Index gemacht, gibt er sofort den Wert zurück

foo :: Int -> Integer
foo i = getMe i
    where

        getMe :: Int -> Integer
        getMe x = resultList !! x 

        resultList :: [Integer]
        resultList = map toFib [0..]

        toFib :: Int -> Integer
        toFib 0 = 1
        toFib 1 = 1
        toFib n = getMe (n-2) + getMe (n-1)


--  Nachteile von dieser Implementierung ist dass der Zugriff über den Index statt dem Pattern-Matching aufwendiger ist und bei großen Eingaben
--  e.g 10^5 langsamer ist. Zusätzlichen nutzen wir die Eigenschaft der Fibonacci Zahlen nicht aus, also dass das Ergebnis immer das vorherige ist
--  d.h wir gehen die gesamte Liste durch um zum gewünschten Index zu kommen. 

--  Diese Eigenschaften sind aber sehr einfach durch den folgenden Ausdruck auszunutzen welcher einer der performantesten überhaupt ist:

zfib :: [Integer]
zfib = 0 : 1 : zipWith (+) (tail zfib) zfib

fastFib :: Int -> Integer
fastFib i = zfib !! i

-- ######################################################################################################
-- | Pattern Matching

-- Unter Pattern-Matching versteht man in Haskell die Möglichkeit unsere Datentypen zu
-- 'dekonstruieren' und damit bestimmte Eigenschaften abzufragen:

f :: Int -> Int
f x = 100

-- Was macht dieses 'x' vor dem '='? Es sagt aus, dass für egal welche Eingabe wir die Zahl 100 zurückgeben.

f' :: Int -> Int
f' 1 = 2
f' x = 100

-- Jetzt geben wir die Zahl 2 zurück, wenn 1 reinkommt. Ansonsten wieder 100.

f'' :: Int -> Int
f'' x = 100
f'' 1 = 2

-- Die Reihenfolge macht einen Unterschied! Es wird von oben nach unten durchgegangen. Der Fall 'x' fängt
-- die 1 ab und gibt trotzdem 100 zurück!

-- Das gleiche geht natürlich auch mit Strings:

g :: String -> String
g "hallo" = "welt!"
g str     = "irgendwas"

-- Wir geben nur dann "welt!" zurück, wenn "hallo" reinkommt. Ansonsten "irgendwas".

-- Betrachten wir nun etwas komplexere Datentypen - Listen:

h :: [Int] -> Int
h [] = 0
h xs = 1

-- Hier haben wir den gleichen Fall wie oben, wenn die Liste leer ist, geben wir 0 zurück, ansonsten 'matchen'
-- wir gegen alles, bennen es xs und geben dann trotzdem 1 zurück. Dieser Fall passiert oft, dass uns die Eingabe egal ist
-- und deswegen können wir folgendes schreiben:

h' :: [Int] -> Int
h' [] = 0
h' _  = 1

-- Damit bennen wir die Eingabe nicht.

-- Schauen wir uns folgende Funktion an:

i :: [Int] -> Int
i []        = 0
i [x]       = 1
i [x,y]     = 2
i [x,y,z]   = 3
i [_,_,_,_] = 4

-- Diese gibt bei leerer Liste die 0 zurück. Bei GENAU einem Element 1, bei GENAU zwei Elementen die 2
-- und bei GENAU 3 Elementen die 3 zurück.

-- Wenn man eine feste Anzahl von Elementen in einer Liste 'matchen' will, benutzt man eckige Klammern.

i' :: [Int] -> Int
i' []     = 0
i' (x:xs) = 2

-- Diese Funktion gibt bei der leeren Liste die 0 zurück. Wenn sie aber MINDESTENS 1 Element hat, die 2.
-- An dieser Stelle wissen wir gar nicht was xs ist, es kann leer sein, oder unendlich lang, wir schauen uns nur
-- das erste Element davon an.

-- Wenn man gegen eine ungewisse Anzahl von Elementen matchen will, macht man das mit normalen Klammern und (:).

j :: [Int] -> Int
j (x:y:xs) = 10
j (x:xs)   = 5

-- Diese Funktion gibt uns bei MINDESTENS zwei Elementen 10 zurück. Wenn die Liste aber MINDESTENS ein Element hat, die 5.
-- Wir haben aber bereits kennengelernt, dass wir von oben nach unten die Pattern durchlaufen.

-- Der einzigste Fall wie wir (x:y:xs) nicht treffen und (x:xs) treffen ist wenn die Liste genau ein Element hat.
-- Das heißt wir können die Funktion so umschreiben:

j' :: [Int] -> Int
j' (x:y:xs) = 10
j' [x]      = 5

-- Übung: Was kommt hier raus? (Zur Überprüfung im GHCi ausprobieren)

u1 :: Int -> Int
u1 1 = 2
u1 x = 3
u1 _ = 5

-- > u1 10
-- ?

u2 :: [Int] -> Int
u2 (1:xs) = 1 + u2 xs
u2 (x:xs) = 10
u2 _      = 100

-- > u2 [1,2]
-- ?

u3 :: String -> Int
u3 ('h':_)      = 2
u3 ('h':'a':xs) = 5
u3 (x:xs)       = 7
u3 xs           = 9
u3 []           = 11

-- > u3 []
-- ?

-- > u3 "hallo"
-- ?


-- | Foliensatz 5: Folie 31+
-- ######################################################################################################

-- | Unterschiede zw. den verschiedenen Typen:

type Something = String

-- | 'type' gibt einem Typ einen neuen Namen, anders gesagt ein Typsynoym

-- goo :: String -> String
goo :: Something -> Something
goo "Hello" = "Hey you!"
goo _       = "No hello for you!"

-- ########################################

-- Datentypname
-- \         / 
--  \       /
--   |     |   Konstruktorname    Argumente
--   |     |   \           _/    /  ______/
--   |     |   |          |  ___/  /
--   |     |   |          | |     |
data Zeitung = Sueddeutsche String 
             | Spiegel      String   -- zweiter Konstruktor
             | KeineAhnung           -- dritter Konstruktor ohne Argument

-- | 'data' lässt uns einen neuen Typ erstellen, der Konstruktoren besitzt. Diese können einen Inhalt haben, oder auch nicht
--   Dieser Datentyp, wie alle anderen Funktionen in Haskell auch, wird, lazy ausgewertet.

-- Regeln die man wissen muss:
--
-- * Datentypname wird groß geschrieben
-- * Konstruktoren werden groß geschrieben
-- * Mehrere Konstruktoren werden mit '|' separiert
-- * Argumente werde mit Leerzeichen nach dem Konstruktor separiert
-- * Man kann automatisch gegen solche Datentypen Pattern-Matchen - dafür braucht man KEINE Eq Instanz. Der Fall aus der Hausaufgabe
--   mit Num a => a ist eine Ausnahme, die später bei Typklassen angesprochen werden kann
-- * Bei mehr einem oder mehr Argumente braucht man leider Klammern beim Pattern-Matchen

inhalt :: Zeitung -> String
inhalt (Sueddeutsche ih) = ih
inhalt (Spiegel      _ ) = "Was auch immer der Spiegel schreibt"
inhalt KeineAhnung       = "Kein Inhalt angegeben"


-- Wenn wir folgendes aufrufen:

-- λ> inhalt (Spiegel "Böehmermann")
-- "Was auch immer der Spiegel schreibt"

-- Der String "Böehmermann" nicht ausgewertet, wir mit einer Wildcard dagegen matchen.
--
-- Das können wir einfach ausprobieren indem wir ein 'undefined' reinpacken und schauen ob er diesem Thunk auswertet

-- λ> inhalt (Spiegel undefined)
-- "Was auch immer der Spiegel schreibt"

-- Wir können uns auch die Konstruktoren genauer anschauen:
--
-- λ> :t Spiegel
-- Spiegel :: String -> Zeitung
--
-- λ> :t KeineAhnung
-- KeineAhnung :: Zeitung
--
-- Man kann sehr gut sehen, dass die Konstrukoren nichts anderes als Funktionen sind, die groß geschrieben werden

-- Um uns das Leben einfacher zu machen, kann man die Argumente der Konstrukoren benennen. Das nennt sich RecordSyntax

data Orc = Orc { ap :: Int, mp :: Int, schrei :: String }
           Orc Int Int String

orc :: Orc
orc = Orc { ap = 10, schrei = "Uarhhhhg!", mp = 1 }

-- Man kann damit sehr einfach neue Ausdrücke erstellen, ohne den gesamten kopieren zu müssen
-- Die Reihenfolge muss nicht eingehalten werden

wütenderOrc :: Orc
wütenderOrc = orc { schrei = "UARGHHHHH!!!!", ap = 15 }

-- Damit müssen wir den Konstruktor nicht mehr per Hand matchen, sondern können seine Argumente mit den 'gettern' rausholen
--
istWütend :: Orc -> String
istWütend orc
  | length (schrei orc) >= 10 = "Orc ist sehr wütend!!"
  | otherwise                 = "Noch gehts."

-- λ> :t schrei
-- schrei :: Orc -> String

-- λ> :t ap
-- ap :: Orc -> Int

-- ########################################

newtype Drache = Drache { farbe :: String }

-- | 'newtype' hat zwei Einschränkungen die 'data' nicht hat. Es darf nur einen Konstruktor und ein Feld geben. Dieser wird während der Compilezeit
--   entpackt, d.h es ist mehr syntactic sugar für den Programmierer, aber der Compiler sieht nur den entpackten Wert 'farbe'.

drache = Drache { farbe = "schwarz" }

fehlerDrache = Drache { farbe = undefined }

farbeVomDrachen :: Drache -> String
farbeVomDrachen (Drache _) = "Dieser Drache hat irgendeine Farbe!"

-- λ> farbeVomDrachen fehlerDrache 
-- "Dieser Drache hat irgendeine Farbe!"
-- 
-- Warum wirft uns das aber keinen Fehler? (das Beispiel aus der Vorlesung ist falsch)
--
-- Evtl. ist die GHC Version verältet, aber ich kann mir sonst nicht erklären wie diese Folie zustande gekommen ist
--
-- Schauen wir uns die offiziellen docs an (https://wiki.haskell.org/Newtype)

data Foo1 = Foo1 Int    -- Defines Foo1 constructor that lazily refers to an Int
data Foo2 = Foo2 !Int   -- Defines Foo2 constructor that strictly refers to an Int
newtype Foo3 = Foo3 Int -- Defines Foo3 constructor that has the same representation as Int
 
-- Argument is lazy and ignored, so 
-- undefined does not cause failure since
-- the contructor pattern match succeeds.
x1 = case Foo1 undefined of
     Foo1 _ -> 1    -- 1
 
-- Argument is strict (because of !), so
-- undefined does cause failure.
x2 = case Foo2 undefined of
     Foo2 _ -> 1    -- undefined
 
-- The newtype behaves like Int, see yInt below
x3 = case Foo3 undefined of
     Foo3 _ -> 1    -- 1
 
-- Constructor pattern match fails
y1 = case undefined of
     Foo1 _ -> 1    -- undefined
 
-- Constructor pattern match fails
y2 = case undefined of
     Foo2 _ -> 1    -- undefined
 
-- The newtype behaves like Int, there is no
-- constructor at runtime.
y3 = case undefined of
     Foo3 _ -> 1    -- 1

-- äquivalent zu dem Ausdruck y3
y4 = case undefined of
          _ -> 1  -- 1
 
-- Demonstration of Int behavior
int :: Int
int = undefined
 
yInt = case int of
       _ -> 1                   -- 1


data Handymarken = Nokia { modell :: Int }

data MBool = MTrue { get :: MyString }

newtype MyString = MyString2 { getString :: String }


data Maybe a = Just a | Nothing

-- | Der wichte Schritt passiert in den Fällen von y2 -> y3. Neben dem unären Konstroktor + Feld verhält sich ein newtype genauso wie
--   primitiver Typ. Man sollte neben den beiden Einschränkungen mitnehmen, dass so ein Typ KEIN overhead für die Instanziierung mit sich trägt.

-- Foliensatz 5: 40+
-- ######################################################################################################

-- Ein Binärbaum besteht aus Knoten (Äste ist auf englisch zu lang zum Schreiben) und Blättern.
-- Ein Knoten enthält einen Wert und einen linken und rechten Knoten zu einem weiteren Binärbaum.
-- Ein Blatt ist das Endstück von einem Knoten und enthält in unserem Beispiel keinen Wert (manche Implementierungen jedoch schon)

-- Datentypname  Typparameter
-- \      ____/  \  _______/
--  \    / _______\/
--   |  | |
--   |  | |
--   |  | |   Konstruktorname  Argumente
--   |  | |   \     _______/  |         |
--   |  | |   |    |   _______|         |
--   |  | |   |    |  |                 |
data Baum a = Knoten (Baum a) a (Baum a)
            | Blatt -- ------------------------ zweiter Konstruktor ohne Argumente
    deriving Show   -- ------------------------ automatisch erstellte Instanz damit wir uns auf der Konsole den Datentyp anschauen können


-- Was bedeutet dieses 'a'?
--
--    Das ist ein sog. Typparameter. Dafür können wir jeden Datentyp einsetzen, ob es nun Int, String, Char, oder [(Char, Int)] ist.
--    Es nennt sich parametrischer Polymorphismus, wenn wir solche Typsubstitutionen machen können.

-- Das erste und dritte Argument von Knoten ist wieder eine Referenz auf sich selber - also wieder ein Binärbaum.

-- Nicht vergessen dass die Konstruktoren trotzdem Funktionen sind!
--
--    λ> :t Knoten
--    Knoten :: Baum a -> a -> Baum a -> Baum a

-- Zum rumspielen ist hier auf der Baum in RecordSyntax. Was hat die Funktion 'value' wohl für ein Typ? (selber nachschauen)

data Tree a = Node { lnode :: Tree a, value :: a, rnode :: Tree a }
            | Leaf
    deriving Show

-- Binärbäume sind eine der einfachsten Datenstrukturen die jeder Programmieren gesehen haben muss. Sie eignen sich
-- sehr gut um das Verständnis von Datentypen in Haskell zu schulen, weil sie rekursiv aufgebaut sind.

-- Im folgenden machen wir nur Beispiele mit Baum Int, aber man kann natürlich jedem anderen Typ einsetzen

blatt :: Baum Int
blatt = Blatt

tiefe1 :: Baum Int
tiefe1 = Knoten blatt 1 blatt

--       Knoten 1
--        /    \
--       /      \
--    Blatt    Blatt

tiefe2 :: Baum Int
tiefe2 = Knoten tiefe1 2 blatt

--            Knoten 2
--              /    \
--             /      \
--       Knoten 1     Blatt
--        /    \
--       /      \
--    Blatt    Blatt

-- | Wie können wir nur Funktionen für diesen Datentyp schreiben?

-- Versuchen wir eine Funktion zu schreiben, die nachschaut ob ein Element in dem Baum vorhanden ist

belem :: Int -> Baum Int -> Bool
belem e Blatt = False       -- Wenn wir am Ende eines Astes angekommen sind, ist das Element offensichtlich nicht auf diesem Ast
belem e (Knoten left value right)
    | e == value = True     -- Element gefunden?
    | otherwise  = (belem e left) || (belem e right) -- Falls nicht, gehe die restlichen Teilbäume durch und verknüpfe sie mit dem logischen Oder

-- Foliensatz 5, Folie 45 ist übrigens falsch an dieser Stelle - man kann genausogut folgendes schreiben:

belem' :: Int -> Baum Int -> Bool
belem' e Blatt = False
belem' e (Knoten left value right) = e == value || belem' e left || belem' e right

-- | Jetzt will ich die Tiefe des Baums bestimmen! (Die Tiefe ist definiert als längsten Ast in einem Baum)

blength :: Baum Int -> Int
blength Blatt = 0
blength (Knoten left _ right) = 1 + max (blength left) (blength right)

-- Die Funktion 'max' gibt mir das Maximum der beiden Werte zurück, d.h der längere Ast wird gewinnen!


-- | Was ist wenn wir aber nun einen Breitendurchlauf machen wollen? (oft gestellte Bewerbungsfrage + Klausurfrage aus 2014)
--   Das bedeutet dass wir von oben von links nach rechts durch die Elemente durchlaufen (unten nummeriert)

tiefe3 :: Baum Int
tiefe3 = Knoten tiefe2 3 tiefe1

--                                        Knoten 3    (Nr.1)
--                                       ____/ \_________
--                       _______________/                \
--                      /                                \
--               Knoten 2    (Nr.2)                Knoten 1  (Nr.3)
--                 /    \                            /  \_
--                /      \                          /     \
--    (Nr.4) Knoten 1     Blatt   (Nr.5)  (Nr.6) Blatt    Blatt  (Nr.7)
--           /    \                                     
--          /      \                                    
-- (Nr.8) Blatt    Blatt   (Nr.9)

-- Damit wir das Ergebnis sehen, sammele ich hier die Elemente einfach auf und gebe sie zurück

breadthElements :: Baum Int -> [Int]
breadthElements baum = go [baum]
    where
        go :: [Baum Int] -> [Int]
        go                             [] = []
        go ( Blatt                   :xs) = go xs
        go ((Knoten left value right):xs) = value : go (left:right:xs)

-- Hier benutze ich einen Trick - ich verpacke den Baum als Liste! Dadurch gewinne ich die Möglichkeit
-- die Teilbäume in genau der Reihenfolge an die Liste dranzuhängen wie ich will. Damit lege ich fest,
-- wie ich den Baum durchlaufe.

-- Das ist am Anfang immer etwas komisch so rekursiv zu denken, aber versucht den Algorithmus im Kopf durchzulaufen.

-- Bonus - Wie macht man diese Aufgabe endrekursiv?

-- | Versuchen wir nun den Tiefendurchlauf zu machen (dieser geht immer den linksten Ast zuerst durch)
--
--                                        Knoten 3    (Nr.1)
--                                       ____/ \_________
--                       _______________/                \
--                      /                                \
--               Knoten 2    (Nr.2)                Knoten 1  (Nr.7)
--                 /    \                            /  \_
--                /      \                          /     \
--    (Nr.3) Knoten 1     Blatt   (Nr.6)  (Nr.8) Blatt    Blatt  (Nr.9)
--           /    \                                     
--          /      \                                    
-- (Nr.4) Blatt    Blatt   (Nr.5)
--
--
depthElements :: Baum Int -> [Int]
depthElements Blatt = [0]       -- hier hab ich eine '0' hinzugefügt, damit man die Reihenfolge besser erkennt
depthElements (Knoten left value right) = value : depthElements left ++ depthElements right

-- Die Idee ist hier einfach den linken Ast zuerst auszuwerten, dann passiert das Backtracking automatisch!
-- Versucht für jeden Knoten im Diagramm oben die Funktion durchzulaufen und dann sieht man wie sich die
-- Reihenfolge von alleine ergibt.

-- Mögliche Übungen:
--
-- *) schreibe die Funktionen in Record-Syntax um
-- *) forme die Funktionen in die Endrekursive Form
-- *) mach die Funktionen nicht von Int abhängig, also z.B depthElements soll die Typsignatur 'Baum a -> [a]' haben