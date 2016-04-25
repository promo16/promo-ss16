
-- palindrom.txt mit Palindromtest und Laenge der Worte auf Standardausgabe ausgeben --

-- Ausführung in der Kommandozeile:
-- $ runghc palindrom-c.hs
-- ODER
-- $ stack runghc palindrom-c.hs
-- ODER in GHCi
-- > :l palindrim-c.hs
-- > main
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc palindrom-c.hs -o palprog
-- $ ./palprog

-- Falls ihr die Datei kompiliert, dann wird NUR die 'main' Funktion aufgerufen, sonst nichts.
-- Deshalb alle Aufrufe von Funktionen in die 'main' packen. Um die anderen Lösungen auszuführen,
-- müsst ihr diese dann 'main' nennen und die vorherige Funktion anders nennen.

-- WICHTIG: Die Datei 'palindrom.txt' muss im selben Verzeichnis sein, wo ihr das Programm aufruft!

import System.IO

-- | Mit Handles
--
main :: IO ()
main = do
    handle <- openFile "palindrom.txt" ReadMode
    input  <- hGetContents handle
    let isPalindrom str =
            if reverse str == str
                then " ist ein Palindrom"
                else " ist kein Palindrom"
        parsed = [l ++ isPalindrom l ++ " und hat die Länge " ++ show (length l) | l <- lines input ]
    putStrLn (unlines parsed)
    hClose handle

-- Erklärung:
--
-- Z.27,28,34,35 sind analog zur a)
--
-- Hier müssen wir lediglich auf die Palindrom-Eigenschaft prüfen, die definiert ist als
--
--    reverse str = str
--
-- Wir definieren uns eine Funktion mit dem Namen isPalindrom, die basieren darauf ob die Eigenschaft
-- zutrifft uns einen String zurückgibt, der dies aussagt.
--
-- Nun führen wir diese Funktion auf jedem 'l' aus und sind bereits fertig!


-- | Die schöne Art und Weise wäre eigentlich monadischen (I/O) Code mit nicht-monadischen zu trennen
-- 
main' :: IO ()
main' = do
    input <- readFile "palindrom.txt"
    putStrLn $ parse input

parse :: String -> String
parse str = unlines [l ++ isPalindrom l ++ ' ' : showLen l | l <- lines str]

showLen :: String -> String
showLen str = "hat die Länge " ++ show (length str)

isPalindrom :: String -> String
isPalindrom str =
    let w = if reverse str == str
              then ""
              else "k"
    in "ist " ++ w ++ "ein Palindrom"