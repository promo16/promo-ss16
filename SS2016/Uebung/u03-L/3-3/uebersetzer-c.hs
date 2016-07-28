
-- Woerterbuch-Datei und Original-Datei aus Programm-Argumente einlesen und Uebersetzung auf Standardausgabe ausgeben und in Datei abspeichern --

-- Ausführung in der Kommandozeile:
-- $ runghc uebersetzer-c.hs woerterbuch.txt original.txt
-- ODER
-- $ stack runghc -- uebersetzer-c.hs woerterbuch.txt original.txt
-- IM GHCi wird es Fehler schmeißen, da dort 'getArgs' nicht funktioniert
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc uebersetzer-c.hs -o uber
-- $ ./uber woerterbuch.txt original.txt

import System.Environment
import System.IO

main :: IO ()
main = do
    arg      <- getArgs
    dcontent <- readFile (arg !! 0)
    original <- readFile (arg !! 1)
    let dict       = lines dcontent
        translated = [translate w dict | w <- (words original)]
    putStrLn $ unwords translated
    writeFile ((arg !! 1) ++ "-translated.txt") (unwords translated)

translate :: String -> [String] -> String
translate input dict = 
    let inputT = [(words line) !! 1 | line <- dict, input `elem` (words line)]
    in if null inputT
           then input
           else unwords inputT

-- Erklärung:
--
-- Hier ist wieder alles analog zu b), bis auf das erneute Rauslesen der Files aus der Kommandozeile (Z.18-20) wie in a)
-- sowie das Erstellen eines neuen Files in Z.24
--
-- Was macht writeFile?
--
--    > :t writeFile
--    FilePath -> String -> IO ()
--
-- Es nimmt also wieder ein Pfad, bzw ein Dateinamen, sowie den Input als String den er darin abspeichern will.
-- Wir erstellen diesmal eine neue Datei basierend auf dem Namen des Originals was zu einem etwas blöden Namen führt:
--
-- $ ./uber woerterbuch.txt original.txt
-- $ ls
-- uber
-- woerterbuch.txt
-- original.txt
-- original.txt-translated.txt
--
-- Die Lösung kann man sich selber überlegen. (hint: (h)ooglet 'takeWhile')
--
-- Was einem auffällt ist, dass der File eventuell nicht gleich formatiert bleibt nachdem ihr ihn übersetzt habt. Warum?
-- 
-- Bsp:
-- ##################
-- original.txt:
--    Hallo Welt!
--    Python sucks.
-- ##################
-- original.txt-translated.txt:
--    Hallo Welt! Python sucks.
-- ##################
--
-- Das passiert weil die Funktionen 'words' <-> 'unwords' nicht bijektiv zueinander sind. Schauen wir uns ein paar Beispiele an:
--
-- words "Hallo       Welt" -> ["Hallo", "Welt"]
-- unwords ["Hallo", "Welt"] -> "Hallo Welt"
--
-- Hier sieht man das alle Leerzeichen verschwunden sind, aber nur eins wieder aufgetaucht ist. Das heißt die Beziehung von
-- 'words' <-> 'unwords' ist lediglich surjektiv.
--
-- Die \n-Character werden dabei auch gelöscht.
--
-- words "Hallo Welt\nPython sucks." -> ["Hallo", "Welt", "Python", "sucks."]
-- unwords ["Hallo", "Welt", "Python", "sucks."] -> "Hallo Welt Python sucks."
--

