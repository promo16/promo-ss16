
-- woerterbuch.txt und original.txt einlesen und Uebersetzung auf Standardausgabe ausgeben --


-- Ausführung in der Kommandozeile:
-- $ runghc uebersetzer-b.hs
-- ODER
-- $ stack runghc -- uebersetzer-b.hs
-- IM GHCi wird es Fehler schmeißen, da dort 'getArgs' nicht funktioniert
-- ODER man kompiliert sich die Datei und führt sie aus:
-- $ ghc uebersetzer-b.hs -o uber
-- $ ./uber

import System.Environment
import System.IO

main :: IO ()
main = do
    content  <- readFile "woerterbuch.txt"
    original <- readFile "original.txt"
    let dict       = lines content
        translated = [translate w dict | w <- (words original)]
    putStrLn $ unwords translated

translate :: String -> [String] -> String
translate input dict = 
    let inputT = [(words line) !! 1 | line <- dict, input `elem` (words line)]
    in if null inputT
           then input
           else unwords inputT


-- Erklärung:
--
-- Z.20: Wir lesen das File ein was wir übersetzen wollen.
--
-- Z.22: Wir spalten das File in einzelne Wörter auf und versuchen jedes davon zu übersetzen.
-- Z.23: Wir geben alles aus!
--
-- Wahrscheinlich hört sich dieses Problem erstmal etwas schwierig an, aber man muss lediglich die
-- Problemstellung auf den kleinsten Nenner bringen! Hier muss man eine Funktion schreiben die ein
-- Wort übersetzt.
--
-- Die Funktion 'translate' nimmt unser Wort und das Wörterbuch und versucht analog wie in a) das Wort zu übersetzen.
-- Falls es keine Übersetzung gefunden hat, gibt es uns das Ausgangswort zurück.
--

