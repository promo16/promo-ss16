
-- palindrom.txt mit Laenge der Worte auf Standardausgabe ausgeben --

-- Ausführung in der Kommandozeile:
-- $ runghc palindrom-b.hs
-- ODER
-- $ stack runghc palindrom-b.hs
-- ODER in GHCi
-- > :l palindrim-b.hs
-- > main
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc palindrom-b.hs -o palprog
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
    let ls     = lines input
        parsed = [l ++ " hat die Länge " ++ show (length l) | l <- ls ]
    putStrLn (unlines parsed)
    hClose handle

-- Erklärung:
--
-- Z.25,26 und 29,30 sind äquivalent zur a)
--
-- Wir lesen nun alle Palindrome aus dem File raus und kriegen sie als String unter dem Namen 'input'
--
-- Da wir noch nichts anderes kennen, müssen wir mit einer List-Comprehention auf jede Zeile zugreifen
-- Dafür splitten wir mit 'lines' unseren input in [String], wobei jeder String eine Zeile ist.
--
-- Nun greifen wir auf jedes Element mit 'l' zu und berechnen die Länge mit 'length l'
-- Das ist aber leider ein Int. Mit 'show (length l)' machen wir es wieder zu einem String und
-- konkatinieren das Ganze.
--
-- Um es nun aber wieder Zeile für Zeile auszugeben müssen wir wieder '\n'´s zwischen alle Strings setzen.
-- Das machen wir mit 'unlines parsed' und geben es wie gewohnt mit 'putStrLn' aus.


-- | Ohne Handles
--
main' :: IO()
main' = do
    input <- readFile "palindrom.txt"
    let parsed = [l ++ " hat die Länge " ++ show (length l) | l <- lines input ]
    putStrLn (unlines parsed)


-- | Die schöne Art und Weise wäre eigentlich monadischen (I/O) Code mit nicht-monadischen zu trennen
--
main'' :: IO ()
main'' = do
    input <- readFile "palindrom.txt"
    putStrLn $ parse input

parse :: String -> String
parse str = unlines [l ++ ' ' : showLen l | l <- lines str]

showLen :: String -> String
showLen str = "hat die Länge " ++ show (length str)





