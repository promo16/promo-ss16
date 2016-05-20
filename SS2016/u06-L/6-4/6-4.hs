  -- Aufgabe 6-4
--
--
-- Wer bei e) und f) aus dem Blatt kopiert muss die single quotes dementsprechend anpassen `` bei `mod` und '' bei '3'

-- a)

aoo :: a -> Bool
aoo x = if aoo x == aoo x
            then aoo x
            else aoo x > aoo x

-- Warum kein Eq, bzw Ord?
-- Bool hat Instanzen den diesen Typklassen.

-- Was macht diese Funktion?
-- Sie terminiert nicht.

-- b)

boo :: Num a => a -> Bool
boo x = if boo(x) == boo(x)
            then boo(x)
            else boo (x + 1) > boo (x + 2)

-- Warum kriegen wir Bool zurück?
-- Im 'else'-Fall machen wir einen Vergleich mit '>',
-- der kann eben True oder False sein.

-- Warum kein Eq, Ord?
-- Bool hat Instanzen in diesen Typklassen

-- Warum Num?
-- Weil wir zu dem Argument 'x' 1 bzw. 2 addieren

-- c)

coo :: (Ord a, Num c) => a -> a -> c -> c
coo x y z = if x > y then z else z + 1

-- Warum Ord?
-- Weil wir 'x' und 'y' mit '>' vergleichen

-- Warum a -> a -> b -> b und nicht a -> c -> b -> b
-- Alles was vergleichen wird, muss vom selben Typ sein

-- Warum Num?
-- Weil wir auf 'z' 1 addieren

-- d)

-- doo :: (Eq a, Num a) => a -> a

doo :: (Eq a, Num a) => a -> a
doo 0 = 1
doo n = n * doo (n - 1)

-- Warum Eq?
-- Weil wir auf '0' pattern-matchen

-- Warum Num?
-- Weil wir eine Zahl zurückgeben auf die wir (*) anwenden

-- e)

eoo :: Integral a => [a] -> [a]
eoo x = [y | y <- x, y `mod` 2 == 0]

-- Warum Integral?
-- Weil wir `mod` auf Elemente aus dieser Liste benutzen

-- Warum [a]?
-- Weil wir in einer List-Comprehention das Argument so benutzen, dass es signalisiert, dass es eine Liste sein muss

-- f)

foo :: Char -> Bool
foo = (\x -> x == '3')

-- Warum kein Eq?
-- Bool hat eine Tnstanz in Eq

-- Was bedeutet (\x -> ...)
-- Wir abstrahieren die Variable x und bilden sie auf etwas ab.
-- Einfacher gesagt - es ist eine andere Schreibweise für 'foo x = ...'
