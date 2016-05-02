
-- palindrom.txt mit Palindromtest und Laenge der Worte auf Standardausgabe ausgeben --

-- Ausführung in der Kommandozeile:
-- $ runghc palindrom-a.hs
-- ODER
-- $ stack runghc palindrom-a.hs
-- ODER in GHCi
-- > :l palindrim-a.hs
-- > main
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc palindrom-a.hs -o palprog
-- $ ./palprog

-- Falls ihr die Datei kompiliert, dann wird NUR die 'main' Funktion aufgerufen, sonst nichts.
-- Deshalb alle Aufrufe von Funktionen in die 'main' packen. Um die anderen Lösungen auszuführen,
-- müsst ihr diese dann 'main' nennen und die vorherige Funktion anders nennen.


-- WICHTIG: Die Datei 'palindrom.txt' muss im selben Verzeichnis sein, wo ihr das Programm aufruft!

import System.IO

-- | Ohne unnötige Handles
--

main :: IO ()
main = do
    input <- readFile "palindrom.txt"
    putStrLn input

-- Erklärung:
-- 
--  readFile hat den folgenden Typ:
--
--          FilePath -> IO String
--
--  FilePath ist nur ein Typsynonym für String. D.h es hat in Wirklichkeit den Typ
--
--          String   -> IO String
--
--  Die Funktion öffnet ein File und holt sich den Inhalt verpackt als String raus
--
--  Jetzt geben wir ihn auf dem Standartoutput aus mithilfe von putStrLn
--
--
--  Falls an dieser Stelle Fragen kommen, wie z.B was ist IO, wieso machen wir diesen '<-', bitte gedulden
--  oder in die Übung kommen und nachfragen, da es zu viel Text einnehmen würde dies mit hier mit Analogien
--  zu erklären.

-- | Mit Handles
--
main' :: IO ()
main' = do
    handle <- openFile "palindrom.txt" ReadMode
    input  <- hGetContents handle
    putStrLn input
    hClose handle

-- Erklärung:
--  
--  Hier passiert das gleiche wie in der oberen Funktion, nur etwas mit etwas mehr Arbeit
--  Wir öffnen den File mit dem sog. ReadMode (für mehr Info dazu (h)ooglet IOMode)
--
--  Das ist im Prizip die Berechtigung mit denen ihr das File öffnet - hier könnt ihr nur lesen.
--
--  > :t openFile
--  FilePath -> IOMode -> IO Handle
--
--  Was ist nun diese Handle? Man kann es sich als Zugriffspunkt für die Datei vorstellen. Ich kann
--  in ihn reinschreiben und rauslesen.
--
--  > :t hGetContents
--  Handle -> IO String
--
--  Diese Funktion liest alles aus dem Handle raus was geht und liefert uns das Ergebnis verpackt als String.
--
--  Nun geben wir ihn wieder auf den Standartoutput aus, dürfen aber eine Sache nicht vergessen. Das Handle muss
--  noch geschlossen werden, da sonst kein anderes Programm darauf Zugriff hat, weil es glaubt dass euer Programm
--  ihn noch benutzt!
--
--  > :t hClose
--  Handle -> IO ()

