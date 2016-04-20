-- Bemerkung: Bei allen Beispielen ist das die wahrscheinlich minimalste Definition!
--            Es braucht kein einziges if-then-else, da das alles durch (==) bereits bool'sche Ausdrücke ist.
--            

-- Aufgabe 2-2 a --
alleGleich :: Eq a => a -> a -> a -> Bool
alleGleich x y z = x == y && y == z

-- Bemerkung: (==) ist eine Äquivalenzrelation und dadurch transitiv. 
--            x == y && x == z && y == z zu überprüfen ist unnötig:


-- Aufgabe 2-2 b --
ungerade :: Integral a => a -> Bool
ungerade  x = odd x

ungerade' x = x `mod` 2 == 1

ungerade''   x = mod x 2   == 1

ungerade'''  x = x `mod` 2 /= 0

ungerade'''' x = mod x 2   /= 0

ungerade''''' x = not (even x)


-- Erklärung:
--
--     Die Infix Schreibweise ist legilich zur Lesbarkeit gedacht und wird durch
--     die `` (Backquotes) eingeleitet. Dadurch kann man Funktionen die zwei Argumente nehmen
--     zwischen die Argumente setzen.

--     4 `div` 2 => 2

--     Die not Funktion macht aus einem bool'schen Ausdruck das Gegenteil:

--     not True  => False
--     not False => True


-- Aufgabe 2-2 c --
gerade :: Integral a => a -> Bool
gerade   x = even x

gerade'  x = x `mod` 2 == 0

gerade'' x = mod x 2   == 0

gerade'''   x = x `mod` 2 /= 1

gerade''''  x = mod x 2 /= 1

gerade''''' x = not (odd x)