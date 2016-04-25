
-- hochdeutsches Wort aus der Standardeingabe einlesen und bayrische Uebersetzung erfragen --

-- Ausführung in der Kommandozeile:
-- $ runghc woerterbuch-a.hs
-- ODER
-- $ stack runghc woerterbuch-a.hs
-- ODER in GHCi
-- > :l woerterbuch-a.hs
-- > main
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc woerterbuch-a.hs -o wortb
-- $ ./wortb

main :: IO ()
main = do
    putStrLn "Deutsches Wort:"
    dw <- getLine
    putStrLn "Bayrisches Wort:"
    bw <- getLine
    if null dw || null bw
        then do
            return ()
        else do
            putStrLn $ '`':dw ++ '`':" heißt im bayerischen " ++
                       '`':bw ++ "`"
            main

-- Erklärung:
--
-- Wir müssen an dieser Stelle drei Sachen erklären:
--
-- > :t getLine
-- IO String
--
-- Diese Funktion liest deine Eingabe von der Standarteingabe ein nachdem du mit ENTER bestätigst.
-- Sie liefert dir diese Eingabe als String.
--
-- Nachdem wir unsere Eingabe getätigt haben, überprüfen wir ob eine von ihnen leer war und falls ja
-- wollen wir das Programm beenden. Das funktioniert mit dem sog. 'return ()'
--
-- Was hier passiert ist dass wir der 'main' Funktion an dieser Stelle sagen wollen dass nichts zu tun ist.
-- Normalerweise würde das ja automatisch passieren (wie in 3-1 a),b),c)), aber da wir ein if-then-else haben
-- hat es nicht den Rückgabewert IO () den main braucht.
--
-- Man erinnere sich - > :t putStrLn
--                     String -> IO ()
--
-- Diesen Rückgabewert erstellen wir mit diesem 'return ()'. Es ist also eine etwas anderes 'return' als wie man es bei
-- Java oder Ähnlichem gewöhnt ist.
--
-- Das letzte was hier anzusprechen ist, ist der rekursive Aufruf in Z.27. Damit wird signalisiert, dass sich die Funktion
-- main selber aufruft und wir sie wieder von oben nach unten durcharbeiten, bis endlich eine der Eingaben leer ist.