
-- hochdeutsches Wort aus der Standardeingabe einlesen und bayrische Uebersetzung erfragen und in Datei woeterbuch.txt speichern --

-- Ausführung in der Kommandozeile:
-- $ runghc woerterbuch-b.hs
-- ODER
-- $ stack runghc woerterbuch-b.hs
-- ODER in GHCi
-- > :l woerterbuch-b.hs
-- > main
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc woerterbuch-b.hs -o wortb
-- $ ./wortb

-- Falls ihr die Datei kompiliert, dann wird NUR die 'main' Funktion aufgerufen, sonst nichts.
-- Deshalb alle Aufrufe von Funktionen in die 'main' packen. Um die anderen Lösungen auszuführen,
-- müsst ihr diese dann 'main' nennen und die vorherige Funktion anders nennen.


import System.IO

main :: IO ()
main = do
    handle <- openFile "woerterbuch.txt" AppendMode
    putStrLn "Deutsches Wort:"
    dw <- getLine
    putStrLn "Bayrisches Wort:"
    bw <- getLine
    if null dw || null bw
        then do
            return ()
        else do
            hPutStrLn handle $ dw ++ ' ':bw
            hClose handle
            main


-- Erklärung:
--
-- Hier sind alle Zeilen bis auf Z.24 und 33 äquivalent zu a)
--
-- Was macht openFile "woerterbuch.txt" AppendMode?
--
--    > :t openFile
--    FilePath -> IOMode -> IO Handle
-- 
-- Wir öffnen wieder ein File im AppendMode, der besagt, dass alles was wir jetzt reinschreiben ganz am Ende
-- angehängt wird.
--
-- Was macht hPutStrLn?
--
--    > :t hPutStrLn
--    Handle -> String -> IO ()
--
-- Diese Funktion nimmt ein Handle und einen Input als String und schreibt ihn in das Handle rein.
-- Wenn wir nun versuchen daraus zu lesen kriegen wir einen Fehler, weil dieser Modus uns das nicht erlaubt.


-- | Der einzige echte Grund warum man per Hand openFile benutzen sollte.
--   Damit verhindert man das Verlangen vom Handle jedes Mal wenn man neue Kombinationen reinschreibt.
--
main' :: IO ()
main' = do
    handle <- openFile "woerterbuch.txt" AppendMode -- Öffnen im AppendMode löscht das File nicht, sondern erlaubt uns
    loop handle                                     -- in der letzte Zeile weiterzuschreiben. Rufen wir unsere Helperfunktion auf

loop :: Handle -> IO ()
loop handle = do
    putStrLn "Deutsches Wort:"
    dw <- getLine
    putStrLn "Bayrisches Wort:"
    bw <- getLine
    if null dw || null bw
        then do
            putStrLn "Saved."
            hClose handle  -- nicht vergessen den Handle zu schließen, damit andere Programme wieder darauf zugreifen können
        else do
            hPutStrLn handle $ dw ++ ' ':bw -- hier schreiben wir etwas in den File rein
            loop handle                     -- und rufen uns rekursiv wieder auf, für den Fall dass wir nochmal eine Kombination reinschreiben wollen
