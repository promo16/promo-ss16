
-- hochdeutsches Wort aus der Standardeingabe einlesen und bayrische Uebersetzung erfragen und in Datei aus Programm-Argument speichern --

import System.IO
import System.Environment


-- Ausführung in der Kommandozeile:
-- $ runghc woerterbuch-c.hs woerterbuch.txt
-- ODER
-- $ stack runghc -- woerterbuch-c.hs woerterbuch.txt
-- IM GHCi wird es Fehler schmeißen, da dort 'getArgs' nicht funktioniert
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc woerterbuch-c.hs -o wortb
-- $ ./wortb woerterbuch.txt

-- Falls ihr die Datei kompiliert, dann wird NUR die 'main' Funktion aufgerufen, sonst nichts.
-- Deshalb alle Aufrufe von Funktionen in die 'main' packen. Um die anderen Lösungen auszuführen,
-- müsst ihr diese dann 'main' nennen und die vorherige Funktion anders nennen.

main :: IO ()
main = do
    args   <- getArgs :: IO [String]
    handle <- openFile (args !! 0) AppendMode
    putStrLn "Deutsches Wort:"
    dw <- getLine
    putStrLn "Bayrisches Wort:"
    bw <- getLine
    if null dw || null bw
        then do 
            hClose handle
        else do
            hPutStrLn handle $ dw ++ ' ':bw
            hClose handle
            main

-- Erklärung:
--
-- Hier sind alle Zeilen gleich zu b) bis auf Z.5,23,24
--
-- In Z.5 importieren wir eine Library namens System.Environment die uns erlaubt die Funktion getArgs zu benutzen.
--
-- Was macht getArgs?
--
--    > :t getArgs
--    IO [String]
--
-- Die Funktion holt uns alle Argumente mit dem das Programm aufgerufen wurde!
--
-- Wenn ich das Programm so aufrufe: ./myprog woerterbuch.txt blub.txt
--
-- Gibt mir getArgs folgendes zurück: ["woerterbuch.txt", "blub.txt"] :: [String]
--
-- In Z.24 rufen wir nun wie gewohnt openFile auf und als FilePath geben wir ihm (args !! 0),
-- was das Elemente an Stelle 0 ist. In unserem Beispiel wäre es "woerterbuch.txt"
--


-- | Der einzige echte Grund warum man per Hand openFile benutzen sollte.
--   Damit verhindert man das Verlangen vom Handle jedes Mal wenn man neue Kombinationen reinschreibt.
--
main' :: IO ()
main' = do
    (fname:_) <- getArgs                   -- Wir holen uns hier mit Pattern-Matching das erste Element raus (kommt in den nächsten Vorlesungen...hoffentlich)
    handle    <- openFile fname AppendMode -- Öffnen im AppendMode löscht das File nicht, sondern erlaubt uns
    loop handle                            -- in der letzte Zeile weiterzuschreiben. Rufen wir unsere Helperfunktion auf

loop :: Handle -> IO ()
loop handle = do
    putStrLn "Deutsches Wort:"
    dw <- getLine
    putStrLn "Bayrisches Wort:"
    bw <- getLine
    if null dw || null bw
        then do
            putStrLn "Saved."
            hClose handle    -- nicht vergessen den Handle zu schließen, damit andere Programme wieder darauf zugreifen können
        else do
            hPutStrLn handle $ dw ++ ' ':bw -- hier schreiben wir etwas in den File rein
            loop handle                     -- und rufen uns rekursiv wieder auf, für den Fall dass wir nochmal eine Kombination reinschreiben wollen
