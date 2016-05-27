-- | Meine eigene Lösung zu der Probleklausur in Promo SS16
-- 

-- A1-1
--
-- Typ von Ausdruck:
--
--    f x = (x + 1 :: Int)
--
-- Lösung:
--
--    Int -> Int
--
-- Warum?
--   *) f ist eine Funktion die ein Argument nimmt, weil vor dem (=) ein 'x' steht (d.h bisher ist der Typ 'a -> b')
--   *) wir verwenden das Argument in unserem Rückgabewert ('a -> a')
--   *) wir addieren zu dem x eine 1. Das beudetet, dass die Funktion '-' für unser x definiert sein muss ('Num a => a -> a')
--   *) der Ausdruck 'x + 1' ist vom Typ Int. Wir setzen für die Typvariable 'a' Int ein. Num Int ist bereits gegeben und wir sind fertig


-- A1-2
--
-- Typ vom Ausdruck:
--
--    (\x -> "b" ++ x)
--
-- Lösung:
--
--    String -> String
--
-- Warum?
--    *) eine anonyme Funktion die ein Argument nimmt, weil wir nach dem '\' nur ein 'x' steht ('a -> b')
--    *) wir sehen dass (++) benutzt wird und auf unser Argument angewandt ('[a] -> [a]')
--    *) das erste Argument von (++) ist ein String (wegen den "_"). (++) kann nur gleichartige Listen verknüpfen und damit sind wir fertig


-- A1-3
--
-- Typ vom Ausdruck:
--
--    g x = x ++ x
--
-- Lösung:
--
--    [a] -> [a]
--
-- Warum?
--    *) g ist eine Funktion die ein Argument nimmt, weil vor dem '=' ein x steht ('a' -> 'b')
--    *) wir sehen dass auf der rechten Seite x im Rückgabewert benutzt wird ('a' -> 'a')
--    *) wir sehen dass (++) benutzt wird. Das funktioniert nur auf Listen. Damit sind wir fertig


-- A1-4
--
-- Typ vom Ausdruck:
--
--    h ["b", "c"]
--
-- gegeben:
--
--    h []     = []
--    h (x:xs) = x ++ x ++ h xs
--
-- Lösung:
--
--    String
--
--
-- Warum?
--    *) Erstmal den Typ von h bestimmen
--    *) Erster Fall sagt uns nichts aus, weil die leere Liste jeden Typ annehmen kann. Wir wissen aber dass es Listen sind ('[a] -> b')
--    *) Zweiter Fall sagt uns nichts neues beim Pattern-Match aus. Wir nehmen das erste Element aus der Liste und machen was damit
--    *) Das Element aus der Liste wird im Ergebnis benutzt und mit (++) verknüpft ('[a] -> a')
--    *) Nun setzen wir ein 'h ["b", "c"]'. Die Liste die eingesetzt wird ist eine Liste von Strings. Wir setzen für 'a' String ein und sind fertig


-- A2-1
--
-- Geben sie eine Definition der Funktion 'summe' die dieser Definition entspricht (einfach über die Summenzeichen für die Argumente schauen)
--
-- Lösung:
--
--    summe :: Int -> Int
--    summe 0 = 0
--    summe n
--        | n >= 1 = summe (n - 1) + n
--

-- A2-2
--
-- Bei negativen Zahlen soll '0' rauskommen:
--
-- Lösung:
--
--    summe :: Int -> Int
--    summe 0 = 0
--    summe n
--        | n >= 1 = summe (n - 1) + n
--        | n <  0 = 0
--

-- A2-3
--
-- Mach diese Funktion endrekursiv.
--
-- Lösung: (Erklärung zu Endrekursion ist auf Blatt 4-1 zu finden) Hier wurde lediglich eine Hilfsfunktion benutzt, ansonsten analog zur 2-2
--
--     summe :: Int -> Int
--     summe n = go n 0
--         where
--             go :: Int -> Int -> Int
--             go 0 acc = acc 
--             go n acc
--                 | n >= 1 = go (n - 1) (acc + n)
--                 | n <  0 = 0


-- A3-1:
--
-- gegeben:
--
--    f n = if n == 0 then 1 else n * f (n-1)
--    doppelt = x ++ x
--    null x = 0

-- 'f 1' in applikativer Reihenfolge auswerten:
--
-- Lösung:
{-

   f 1                                                -- in Lambda Form bringen
   -

=> (\n -> if n == 0 then 1 else n * f (n - 1)) 1      -- einsetzen
   ----------------------------------------------

=> if 1 == 0 then 1 else 1 * f (1 - 1)                -- if-Regel greift, zuerst Bedingung auswerten
      ------

=> if False  then 1 else 1 * f (1 - 1)                -- if auswerten
   -----------------------------------

=> 1 * f (1 - 1)                                      -- nach der applikativen Reihenfolge, immer die innersten Klammern zuerst
         -------

=> 1 * f 0                                            -- in Lambda Form bringen
       -

=> 1 * (\n -> if n == 0 then 1 else n * f (n - 1)) 0  -- einsetzen
       ---------------------------------------------

=> 1 * if 0 == 0 then 1 else 0 * f (0 - 1)            -- if-Regel greift, zuerst Bedingung auswerten
          ------

=> 1 * if True then 1 else 0 * f (0 - 1)              -- if auswerten
       ---------------------------------

=> 1 * 1                                              -- ausrechnen
   -----

=> 1

-}

-- A3-2:
--
-- 'f 1' in normaler Reihenfolge auswerten:
--
-- Lösung:
{-

   f 1                                                      -- in Lambda Form bringen
   -

=> (\n -> if n == 0 then 1 else n * f (n - 1)) 1            -- einsetzen
   ----------------------------------------------

=> if 1 == 0 then 1 else 1 * f (1 - 1)                      -- if-Regel greift, zuerst Bedingung auswerten
      ------

=> if False  then 1 else 1 * f (1 - 1)                      -- if auswerten
   -----------------------------------

=> 1 * f (1 - 1)                                            -- nach der normalen Reihenfolge, immer die äußersten Klammern zuerst
         -------

=> 1 * f (1 - 1)                                            -- in Lambda Form bringen
       -

=> 1 * (\n -> if n == 0 then 1 else n * f (n - 1)) (1 - 1)  -- einsetzen
       ---------------------------------------------

=> 1 * if (1 - 1) == 0 then 1 else (1 - 1) * f ((1 - 1) - 1) -- if-Regel greift, zuerst Bedingung auswerten
          -------

=> 1 * if 0 == 0 then 1 else (1 - 1) * f ((1 - 1) - 1)      -- weiter auswerten
          ------

=> 1 * if True then 1 else 0 * f (0 - 1)                    -- if auswerten
       ---------------------------------

=> 1 * 1                                                    -- ausrechnen
   -----

=> 1

-}

-- A3-3:
--
-- 'null (doppelt x)' in verzögerter Reihenfolge auswerten:
--
-- Lösung:
--
{-

    null (doppelt x)

=>  let a = doppelt x
    in null a                       [(a, doppelt x)]

=>  null a                          [(a, doppelt x)]

=>  (\x -> 0) a                     [(a, doppelt x)]

=>  0                               [(a, doppelt x)]

=>                                  []

-}

-- A3-4:
--
-- 'hd [4..]' in verzögerter Reihenfolge auswerten:
--
-- Lösung (ausführlich):
--
{-
    hd [4..]

=>  let a = [4..]
    in hd a                     [(a, [4..])]

=>  hd a

=>  (\(x:xs) -> x) a            [(a, [4..])]

=>  let (b:bs) = ^a
    in (\(x:xs) -> x) (b:bs)    [(bs, (drop 1 ^a)), (b, head ^a), (a, [4..])]             -- bs referenziert sich hier auf die Liste a ohne das erste Element

=>  ^b                          [(bs, (drop 1 ^a)), (b, head ^a), (a, [4..])]

=>  head ^a                     [(bs, (drop 1 ^a)), (b, head ^a), (a, [4..])]

=>  4                           [(bs, (drop 1 ^a)), (b, head ^a), (a, [4..])]

=>                              []

-}


-- Etwas kürzere Lösung:
{-
    hd [4..]

=>  let (b:bs) = [4..]
    in h (b:bs)                 [(b, head [4..]), (bs, drop 1 [4..])]

=>  h (b:bs)                    [(b, head [4..]), (bs, drop 1 [4..])]

=>  (\(x:xs) -> x) (b:bs)       [(b, head [4..]), (bs, drop 1 [4..])]

=>  ^b                          [(b, head [4..]), (bs, drop 1 [4..])]

=>  4                           [(b, head [4..]), (bs, drop 1 [4..])]

=>                              []

-}


-- A4-1
--
-- gegeben:

data BB a = L | B a | K (BB a) a (BB a)

-- wobei BB ^= Binärbaum
--       L  ^= Leer
--       B  ^= Blatt (?)
--       K  ^= Knoten

-- Ausführliches Beispiel in wiederholungs-folien.hs ab Z.397

-- gefragt: Geben sie einen ausgeglichenen Baum vom Typ Num a => Baum a mit den Werten [0..6] an
--
-- Ich interpretiere einfach mal, dass mit ausgeglichen ein binärer Suchbaum gemeint war, welcher die folgende Eigenschaft hat:
-- 
-- Für jeden Knoten müssen alle Knoten in seinem linkter Teilbaum kleiner sein und im rechten Teilbaum größer sein
--
-- Dafür gibt es ein einfachen Algorithmus:

-- Man nimmt die Mitte der Liste (wenn man keine hat darf man sich entscheiden)
--
-- [0,1,2,3,4,5,6] => 3
--
-- Alles was links davon steht, ist der linke Teilbaum und analog rechts
--
--                  (3)
--                 /   \
--             [0,1,2] [4,5,6]
--
-- Man nimmt jeweils die Mitte der beiden Listen:

-- [0,1,2] => 1
-- [4,5,6] => 5
--
--                 (3)
--             ___/   \___
--            /           \
--          (1)           (5)
--          / \           / \
--         0   2         4   6

-- Und damit ist man fertig. Nun nur noch hinschreiben in der Haskell-Syntax:

-- Wir fangen von oben an:     Knoten  x                             3  y
--                             Knoten (Knoten  a        1  b)        3 (Knoten  c        5  d)
--                             Knoten (Knoten (Blatt 0) 1 (Blatt 2)) 3 (Knoten (Blatt 4) 5 (Blatt 6))
--
-- Nun noch die Konstruktoren so bennenen wie sie in der Aufgabe definiert waren:
--
-- K (K (B 0) 1 (B 2)) 3 (K (B 4) 5 (B 6))

-- A4-2:
--
belem :: Eq a => a -> BB a -> Bool
belem e L         = False            -- Dieser Fall fehlt in der Musterlösung und macht diese damit partiell (und falsch)
belem e (B value) = e == value       -- Wenn wir an einem Blatt sind, müssen wir noch schauen ob der Wert darin vorkommt
belem e (K left value right)
    | e == value = True                              -- Element gefunden?
    | otherwise  = (belem e left) || (belem e right) -- Falls nicht, gehe die restlichen Teilbäume durch und verknüpfe sie mit dem logischen Oder

-- Mehr zu diesen Aufgaben in wiederholungs-folien.hs ab Z.450

