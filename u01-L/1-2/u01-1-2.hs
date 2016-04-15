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
a' = 50 *  100 - 4999 -- 1
-- F: Integer
-- G: Num a => a

-- Erklärung: Wenn etwas von dem Typ (Num a) ist, heißt es dass es ein
-- 'Obertyp' von allen Zahlen ist. (Dazu später mehr in folgenden Vorlesungen)

-- D.h man kann ein (Num a) zu einem Int, Double, etc. 'casten'

-- Beispiel:
--     a' :: Double
--     -1.0

--     a' :: Int
--     -1


-- b)
b  = 50 * (100 - 4999) -- -244950
-- F: Integer
-- G: Num a => a

-- c)
c  = 1 - 200 / 0       -- -Infinity
-- F: Double
-- G: Fractional a => a

-- Erklärung: -199 / 0 => - Infinity

-- d)
fuenf    = 5
-- F: Integer
-- G: Num a => a

-- e)
acht     = 008         -- 8
-- F: Integer
-- G: Num a => a

-- Erklärung: Führende Nullen werden weggelassen


-- f)
x10      = 0x10        -- 16
-- F: Integer
-- G: Num a => a

-- Erklärung:
--     Diese Zahl wird Hexadezimal aufgefasst, also
--     1 * 16^1 + 0 * 16^0 = 16
--     |    ______|
--      \  /
--       ||
--     0x10

-- Beispiel: 0x231 = 561

--     2 * 16^2 + 3 * 16^1 + 1 * 16^0
--  => 512      + 48       + 1
--  => 561


-- g)
fuenf'   = -(-5)       -- 5
-- F: Integer
-- G: Num a => a


-- h)
fuenftel = 1.0 / fromIntegral (fuenf) -- 0.2
-- F: Double
-- G: Fractional a => a


-- i)
i = (fuenf == acht) == (fuenf > acht) -- True
-- F: Bool
-- G: Bool

-- Erklärung:
--     fuenf == acht   => 5 == 8 => False
--     fuenf > acht    => 5 >  8 => False
--     False == False            => True


-- j)
j = fuenf /= 5 -- False
-- F: Bool
-- G: Bool

-- Erklärung: Der (!=) Operator ist leider öfters durch externe Libraries
--     besetzt, deswegen ist ungleich in Haskell (/=)

-- k)
a = 'a'
-- F: Char
-- G: Char


-- l)
bc = "bc"
-- F: String oder [Char]
-- G: String oder [Char]

-- Erklärung: Ein String ist in Haskell eine Liste von Chars, also ist
--     die analoge Schreibweise [Char], ich werde im Folgenden die Listen-Schreibweise
--     benutzen, damit es offensichtlicher wird, dass wir mit verlinkten Listen arbeiten

-- m)
abc = a : bc  -- "abc"
-- F: [Char]
-- G: [Char]

-- Erklärung: (:) hat die Typsignatur
--               'a' : "bc" => "abc"
--                a -> [a] -> [a]
--                |     |
--                 \     \_________________
--                  \_____ ___________      \________
--                        |           |     |        |
-- das heißt er nimmt ein loses Element und eine Liste
-- und gibt uns eine Liste zurück! In unserem Fall ist es
-- 
--            Char -> [Char] -> [Char]
--
-- Was der sog. "Cons"-Operator macht, ist eine Element von vorne
-- an eine Liste anzuhängen. Wichtig ist zu bemerkten, dass das Element
-- vom selben Typ wie die Liste sein müssen:

-- D.h sowas geht nicht: 1 : "test"


-- n)
begruessung = "hallo " ++ "welt!" -- "hallo welt!"
-- F: [Char]
-- G: [Char]

-- Erklärung: (++) hat die Typsignatur
--
--                [a] -> [a] -> [a]
--                 |      |
--                  \_     \______________
--                    \________           \__________
--                    |        |          |          |
-- das heißt er nimmt eine Liste und eine andere Liste
-- und gibt uns eine zusammengefügte Liste zurück! In unserem Fall ist es
-- 
--            [Char] -> [Char] -> [Char]
--
-- Was der "++"-Operator macht, ist einee Liste von vorne
-- an eine andere Liste anzuhängen. Wichtig ist zu bemerken, dass die Listen vom
-- selben Typ sein müssen!

-- D.h Sowas geht nicht: [1,2] ++ "Hallo"


-- o)
begruessung' = begruessung ++ " gut gem" ++ show acht -- "hallo welt! gut gem8"
-- F: [Char]
-- G: [Char]

-- Erklärung: show hat die Typsignatur:
--
--         Show a => a -> String
--
-- das heißt sie nimmt irgendwas von einer Klasse Show (dazu mehr in den folgenden Vorlesungen)
-- und gibt uns ein String ([Char]) zurück.

-- Man kann sich die Klasse Show als eine ToString Methode aus Java vorstellen,
-- für jeden Typ wo eine ToString Methode definiert wurde, kann man diese Funktion aufrufen

-- Die show Funktion bringt also unsere Variablen in eine Stringform. In unserem
-- Fall ist es die Stringdarstellung von der Zahl 8 gewesen, also wurde es zu "8"


-- p)
p = read (show fuenftel) + fuenftel

--    read (show 0.2)    + 0.2
-- => read "0.2"         + 0.2
-- => 0.2                + 0.2
-- => 0.4

-- F: Double
-- G: (Fractional a, Read a) => a

-- Erklärung: read hat die Typsignatur:
--
--          Read a => String -> a
--
-- das heißt sie nimmt irgendein String und versucht ihn in irgendwas umzuwandeln,
-- was von der Klasse Read ist. Es gibt sehr viele Implementierungen für Typen von
-- der Klasse Read. Suchen wir diese mal:

-- ###### Ausblick #######

-- Gehen wir auf: http://hayoo.fh-wedel.de/
-- Suche nach 'Read'
-- Auf das erste Ergebnis klicken!

-- Dann landen wir bei:
--
--     http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Read
--
-- Wir scrollen runter zu den Instanzen und sehen die vielen verschiedenen Typen
-- wofür diese Klasse definiert ist.

-- ###### Zurück zur Aufgabe ######

-- Es gibt bereits eine Implementierung für Double und Float, die beide von der Klasse
-- Fractional sind. Deswegen ist sie sich nicht sicher zu was der String "0.2" werden soll
-- und deswegen sagt er das Ergebnis muss von dieser Klasse sein. Da aber auch verlangt wird
-- das dieser String zu irgendwas Sinnvollem werden soll, muss er auch von der Klasse Read sein.

-- Es ist sehr schlechter Stil den Typ bei 'read' Anweisungen nicht anzugeben und führt
-- im GHCi oft zu Problemen. Deswegen nach jeder 'read' Anweiseung mit :: den Typ hinten
-- dran anhängen.

-- Das Beispiel aus der Übung funktioniert ausschließlich deswegen ohne die Typsignatur
-- weil der Typ durch das (+ fuenftel) vorgegeben wird.

-- Beispiele:

-- read "1" 
-- Fehler! Er kann den Typ nicht inferieren

-- read "1" :: Int
-- 1

-- read "1" + 1
-- 2 :: (Num a, Read a) => a
-- Er kann den Typ inferieren, weil er weiß dass 1 aus 'Num' kommt

-- read "1.5" :: Double
-- 1.5

-- read "1.5 + 1.0" :: Double
-- Fehler! read kann keine komplexen Ausdrücken evaluieren (nicht wie eval aus JS)

-- read "True" :: Bool
-- True

-- read "True" && False
-- False
-- Er kann hier wieder den Typ inferieren, weil das logische UND nur Bool-Werte akzeptiert


-- Wer mehr über Typklassen wissen will, diese Seite gibt einen groben Überblick darüber:
-- http://www.zvon.org/other/haskell/Outputprelude/index.html