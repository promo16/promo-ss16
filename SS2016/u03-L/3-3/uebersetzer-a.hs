-- woerterbuch.txt einlesen und Wort von der Standardeingabe uebersetzen --

-- Ausführung in der Kommandozeile:
-- $ runghc uebersetzer-a.hs woerterbuch.txt
-- ODER
-- $ stack runghc -- uebersetzer-a.hs woerterbuch.txt
-- IM GHCi wird es Fehler schmeißen, da dort 'getArgs' nicht funktioniert
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc uebersetzer-a.hs -o uber
-- $ ./uber woerterbuch.txt

-- Falls ihr die Datei kompiliert, dann wird NUR die 'main' Funktion aufgerufen, sonst nichts.
-- Deshalb alle Aufrufe von Funktionen in die 'main' packen. Um die anderen Lösungen auszuführen,
-- müsst ihr diese dann 'main' nennen und die vorherige Funktion anders nennen.

import System.IO
import System.Environment

-- | Mit Handles
--
main :: IO ()
main = do
    args     <- getArgs
    handle   <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    let dict = lines contents

    input <- getLine
    let translated = [(words line) !! 1 | line <- dict, input `elem` (words line)]

    if null translated
        then print input
        else print $ unwords translated

    hClose handle

-- Erklärung:
--
--  Neue Zeilen sind 26-33:
--
-- (Z.26) Zuerst spalten wir unser Wörterbuch in Zeilen die folgendermaßen aussehen:
-- 
-- [ <deutsches wort> LEERZEICHEN <bayerisches wort>
-- , <deutsches wort> LEERZEICHEN <bayerisches wort>
-- , <deutsches wort> LEERZEICHEN <bayerisches wort>
-- , ...
-- ] 
--
-- (Z.28) Nun lesen wir die Eingabe von dir ein und müssen nun das Wort in unserem Wörterbuch suchen.
--
-- (Z.29) Da wir leider nur List-Comprehentions kennen, müssen wir damit auskommen! Wir iterieren durch jede Zeile
-- in unserem Wörterbuch und schauen ob unsere Bedingung zutrifft.
--
-- Was macht 'input `elem` (words line)'?
--
-- Zuerst spalten wir unsere Zeile mit 'words' nach Leerzeichen, dann kommt raus [<deutsches wort>, <bayerisches wort>] raus.
-- Nun schauen wir ob das Wort was wir zu übersetzen versuchen in dieser Liste vorkommt - falls ja, geben wir
-- einfach stur das bayerische Wort raus.
-- Das machen wir indem wir auf den Index 1 zugreifen mit '(words line) !! 1'
--
-- (Z.31)
-- Nun überprüfen wir nur ob wir eine Übersetzung gefunden haben und geben das jeweilige Ergebnis raus und sind fertig.

-- | Ohne Handles
--
main' :: IO ()
main' = do
    (fname:_) <- getArgs         -- Wie in 3-2 c)
    dict      <- readFile fname  --
    loop $ lines dict            -- Helperaufruf mit unserem Wörterbuch in Zeilen gesplittet


loop :: [String] -> IO ()
loop dict = do
    putStr "Requesting translation to bavarian: "
    input <- getLine
    let translated = [(words line) !! 1 | line <- dict, input `elem` (words line)]
    if null translated
        then putStrLn "Sorry, none found. Exiting..."
        else do
            putStrLn $ unwords translated
            loop dict

