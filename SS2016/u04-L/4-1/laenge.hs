-- Installationsanleitung für criterion
--
-- Gehe in irgendeine übergeordnete Directory, z.B C:/Documents/Uni/SemesterX/Haskell/
-- $ cabal sandbox init
-- $ cabal install criterion -j --disable-tests
--
-- Nun hast du in deinem Ordner und allen Unterordnern diese Library zur Verfügung.
-- Wer ganz viel Mut besitzt darf auch ohne die sandbox installieren.
--
-- Stack User
-- $ stack install criterion
-- 
-- Was ist criterion und wie benutzt man es?
-- 
-- Criterion ist eine sehr gute Benchmarking Bibliothek die es uns erlaubt schöne Graphen
-- in Form von statischen HTML Seiten zu generieren und Aussagen über die wahrscheinliche
-- Effizienz von unseren Funktionen zu treffen.
--
-- Wie benutzt man es?
--
-- a) 'import Criterion.Main' oben im File hinschreiben
-- b) main = defaultMain [
--      bgroup "Gruppenname-1" [ bench "Name der Aktivität" $ whnf (funktion argumente)
--                             , bench "Zweite Aktivität"   $ whnf (funktion2 argumente)
--                             , ...] 
--    , bgroup "Gruppenname-2" [..]
--    , ...
--       ]
--
-- * defaultMain parst für uns automatisch cmd-Argumente
-- * bgroup erlaubt uns die Gruppierung verschiedener Benchmarks, sie werden farblich seperiert in der HTML Seite
-- * > :t bench
--   String -> Benchmarkable -> Benchmark
-- * 'whnf' bedeutet ausgesprochen Weak-Head-Normal-Form und ist eine Art der Evaluation in Haskell (dazu mehr in folgenden Vorlesungen)
--   Umgangssprachlich beudetet sie, dass wir unsere Daten nur sowenig wie möglich auswerten, aber dennoch auswerten.
--   Ich weiß - brainfuck wenn man noch keine Ahnung von Datentypen in Haskell hat...
--
-- * Vollständige Auswertung gibt criterion auch her - namens 'nf' (kann man also statt whnf oben einsetzen)
-- * Wenn man IO Aktionen benchmarken will gibt es die Funktion 'nfIO' (auch statt whnf oben einsetzen)
-- 
--
-- Was bedeutet:
--
-- * linker Graph - ?
-- * rechter Graph - Wie stark steigt der Zeitaufwand pro Durchlauf
--
-- * OLS Regression - eine etwas andere Art die durchschnittliche Zeit zu approximieren
-- * R^2            - Wie sehr weichen die gemessenen Werte von den Voraussichtlichen ab
-- * Mean           - Durchschnittliche Zeit (echt gemessen)
-- * Std            - Durchschnittliche Abweichung 
--
--
-- Wie lässt man es laufen?
--
-- Wir kompilieren:
-- $ ghc <textfile.hs> -o myprog
-- $ ./myprog --output myprog.html
--
-- ODER
-- $ runghc <textfile.hs> --output myprog.html
--
-- ODER
-- $ stack runghc -- <textfile.hs> --output myprog.html
--
-- ODER (hier gibt es aber keine schöne HTML Seite, sondern nur Text auf der Kommandozeile)
-- $ ghci
-- > :l <textfile.hs>
-- > main

{-# LANGUAGE BangPatterns #-}

import Criterion.Main

-- 4-1 a) Laenge einer Liste --

-- | Nicht endrekursiv
--
nLen :: [a] -> Int
nLen []     = 0
nLen (x:xs) = 1 + nLen xs

nLen' :: [a] -> Int
nLen' []     = 0
nLen' (_:xs) = 1 + nLen' xs

-- Erklärung:
--
-- Was bedeutet nicht endrekursiv?
-- 
-- Jede nicht-endrekursive Funktion ist nicht endrekursiv. (siehe Definition nicht Endrekursiv)

-- | Endrekursiv
--
eLen :: [a] -> Int
eLen xs = go 0 xs
    where go :: Int -> [a] -> Int
          go n []     = n
          go n (_:xs) = go (n+1) xs

-- Erklärung:
--
-- Was bedeutet endrekursiv?
--
-- Eine einfache Definition ist die folgende:
--
-- Eine Funktion ist dann endrekursiv, wenn bei dem letzten rekursiven Funktionsaufruf das Ergebnis feststeht.
--
-- Wann ist es denn nicht der Fall? Gehen wir das Beispiel für 'nLen [1,2,3]' durch:

{-

   nLen [1,2,3]                     (wir kommen nun in den zweiten Fall, da die Liste nicht leer ist)
=> nLen (1:[2,3]) = 1 + nLen [2,3]  (erster rekursiver Aufruf)
=> 1 + nLen [2,3]                   
=> 1 + nLen (2:[3]) = 1 + nLen [3]  (zweiter rekursiver Aufruf)
=> 1 + 1 + nLen [3]                 (wir setzen einfach von oben ein und sehen, dass sich die 1'er von vorne sammeln)
=> 1 + 1 + nLen [3] = 1 + nLen []   (dritter rekursiver Aufruf)
=> 1 + 1 + 1 + nLen []              (nun kommen wir in den ersten Fall)
=> 1 + 1 + 1 + 0                    (das war der letzte rekursive Aufruf - aber haben wir hier das Ergebnis?)

  Um das Ergebnis zu bestimmen muss GHC leider noch die 1'er aufsummieren und damit ist unsere Funktion
  nicht endrekursiv, da der letzte rekursive Aufruf nicht zum Ergebnis geführt hat.

  Bei den meisten nicht-endrekursiven Funktionen führt diese Aneinanderreihung von Zwischenergebnissen
  zu sehr großen Performanzeinbußen und wird deswegen in anderen Programmiersprachen extrem geächtet.

  Wenn aber Funktionen in endrekursiven Form vorliegen, können manche Compiler ein sog. TCO (tail-call-optimization)
  durchführen. Bleiben wir einfach dabei, dass es die Funktion annehmbar schnell macht.
  Wer es genauer wissen will: https://en.wikipedia.org/wiki/Tail_call

-}

{-

  Wie schreiben wir nun eine endrekursive Funktion?

  Dies kann man mithilfe von einem Akkumulator machen, schauen wir das am Beispiel von 'eLen [1,2,3]' an:

   eLen [1,2,3]
=> go 0 [1,2,3]
=> go 0 (_:[2,3]) = go (0+1) [2,3]   (erster rekursiver Aufruf)
=> go 1 (_:[3])   = go (1+1) [3]     (zweiter rekursiver Aufruf)
=> go 2 (_:[])    = go (2+1) []      (dritter rekursiver Aufruf)
=> go 3 []        = 3                (fertig!)

   Weil wir unser Ergebnis in einem Argument der Funktion immer abgespeichert und rumgereicht haben,
   hat der letzte rekursive Aufruf zum Ergebnis geführt. Damit wäre diese Funktion end-rekursiv.

   Die Idee bei endrekursiven Funktion ist meist immer folgende:

   * Erstelle eine Hilfsfunktion die zu den normalen Argumenten ein zusätzlichen Wert mitbekommt der dem Ergebnis-Typ
     entspricht (wir wollten die Länge der Liste haben, dsw haben wir die 0 als Startwert gewählt)

   * Speichere das Zwischenergebnis immer in diesem Argument ab und reiche es mit jedem rekursiven Aufruf weiter

-}


-- | sog. BangPatterns erlauben uns wirklich strikte Evaluation (die Ausrufezeichen vor den n's)
--
--   nicht klausurrelevant, aber in den folgenden Vorlesungen wird das vielleicht angesprochen
eLen' :: [a] -> Int
eLen' xs = go 0 xs
    where go :: Int -> [a] -> Int
          go !n []     = n
          go !n (_:xs) = go (n+1) xs

main :: IO ()
main = defaultMain [
    bgroup "recursive" [ bench "nLen 1 whnf" $ whnf nLen  [1..100000]
                       , bench "nLen 2 whnf" $ whnf nLen' [1..100000]
                       , bench "nLen 1 nf"   $ nf nLen  [1..100000]    -- 'nf' wertet strikter aus als whnf
                       , bench "nLen 2 nf"   $ nf nLen' [1..100000]
                       ]
  , bgroup "tail-recursive" [ bench "eLen 1 whnf" $ whnf eLen  [1..100000]
                            , bench "eLen 2 whnf" $ whnf eLen' [1..100000]
                            , bench "eLen 1 nf" $ nf eLen  [1..100000]
                            , bench "eLen 2 nf" $ nf eLen' [1..100000]
                            ]
    ]

-- Interpretation wird live gemacht, da sie stark abweichen kann. Ist eh nur eine schöne Visualisierung
-- damit man sich merken kann, dass TCO (tail-call-optimizaion) was taugt.



{-

Nun kommen wir zum Pattern-Matching:

Unter Pattern-Matching versteht man in Haskell die Möglichkeit unsere Datentypen zu
'dekonstruieren' und damit bestimmte Eigenschaften abzufragen:

f :: Int -> Int
f x = 100

Was macht dieses 'x' vor dem '='? Es sagt aus, dass für egal welche Eingabe wir die Zahl 100 zurückgeben.

f :: Int -> Int
f 1 = 2
f x = 100

Jetzt geben wir die Zahl 2 zurück, wenn 1 reinkommt. Ansonsten wieder 100.

f :: Int -> Int
f x = 100
f 1 = 2

Die Reihenfolge macht einen Unterschied! Es wird von oben nach unten durchgegangen. Der Fall 'x' fängt
die 1 ab und gibt trotzdem 100 zurück!

Das gleiche geht natürlich auch mit Strings:

g :: String -> String
g "hallo" = "welt!"
g str     = "irgendwas"

Wir geben nur dann "welt!" zurück, wenn "hallo" reinkommt. Ansonsten "irgendwas".

Betrachten wir nun etwas komplexere Datentypen - Listen:

h :: [Int] -> Int
h [] = 0
h xs = 1

Hier haben wir den gleichen Fall wie oben, wenn die Liste leer ist, geben wir 0 zurück, ansonsten 'matchen'
wir gegen alles, bennen es xs und geben dann trotzdem 1 zurück. Dieser Fall passiert oft, dass uns die Eingabe egal ist
und deswegen können wir folgendes schreiben:

h :: [Int] -> Int
h [] = 0
h _  = 1

Damit bennen wir die Eingabe nicht.

Schauen wir uns folgende Funktion an:

h :: [Int] -> Int
h []        = 0
h [x]       = 1
h [x,y]     = 2
h [x,y,z]   = 3
h [_,_,_,_] = 4

Diese gibt bei leerer Liste die 0 zurück. Bei GENAU einem Element 1, bei GENAU zwei Elementen die 2
und bei GENAU 3 Elementen die 3 zurück.

Wenn man eine feste Anzahl von Elementen in einer Liste 'matchen' will, benutzt man eckige Klammern.


h :: [Int] -> Int
h []     = 0
h (x:xs) = 2

Diese Funktion gibt bei der leeren Liste die 0 zurück. Wenn sie aber MINDESTENS 1 Element hat, die 2.
An dieser Stelle wissen wir gar nicht was xs ist, es kann leer sein, oder unendlich lang, wir schauen uns nur
das erste Element davon an.

Wenn man gegen eine ungewisse Anzahl von Elementen matchen will, macht man das mit normalen Klammern und (:).

h :: [Int] -> Int
h (x:y:xs) = 10
h (x:xs)   = 5

Diese Funktion gibt uns bei MINDESTENS zwei Elementen 10 zurück. Wenn die Liste aber MINDESTENS ein Element hat, die 5.
Wir haben aber bereits kennengelernt, dass wir von oben nach unten die Pattern durchlaufen.

Der einzigste Fall wie wir (x:y:xs) nicht treffen und (x:xs) treffen ist wenn die Liste genau ein Element hat.
Das heißt wir können die Funktion so umschreiben:

h :: [Int] -> Int
h (x:y:xs) = 10
h [x]      = 5

Übung: Was kommt hier raus? (Wer überprüfen will einfach in GHCi reinladen und nachschauen)

-}

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