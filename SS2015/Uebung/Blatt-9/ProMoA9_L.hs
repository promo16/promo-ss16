module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

-- | A9-2

main :: IO ()
main = do

-- Das sagt der Konsole, dass jedes Mal wenn eine Aktion kommt,
-- die einen Character ausdruckt, wird sie sofort ausgeführt,
-- anstatt auf manchen Systemen zu warten, bis ein "\n\r" kommt

-- => kann hier ignoriert werden
  hSetBuffering stdout NoBuffering

-- 'putStr' druckt einen String ohne ein '\n' auf die Konsole
  putStr "A: "

  a2 <- getLine

-- putStr :: String -> IO ()
--          |___|__________|
--           /  |
--          /   |
--         /    |
  b1 <- putStr "B: "
-- |
--  \___________
--              \
-- String -> IO ()

-- => wir hätten auch nur 'putStr "B: "' schreiben können


-- Wir benennen die Aktionen - führen sie aber nicht aus!
  let b2 = getLine
  let c1 = putStr "C: "

-- wäre analog schreibbar, wenn man unter das 'let' einrückt
--let b2 = getLine
--    c1 = putStr "C: "

  c2 <- getLine
  putStr "D: "


-- an dieser Stelle wird das 'b2' was 'getLine' war,
-- mit dem Ergebnis 'b2' überschattet, was nun ein String ist
-- (sollte man aber trotzdem nicht machen)
  b2 <- b2

-- Wenn man das Argument von 'putStrLn' ein String ist, der aus mehreren
-- Teilen besteht, muss man entweder alles in eine Klammer packen,
-- oder sie mit einem '$' ersetzen
  putStrLn $ "A=" ++ a2 ++ ' ' :
             "B=" ++ b2 ++ ' ' :
             "C=" ++ c2

-- analog
--putStrLn ( "A=" ++ a2 ++ ' ' :
--           "B=" ++ b2 ++ ' ' :
--           "C=" ++ c2 )


-- | A9-3

-- a)

--K_a |- if x then "Akzeptiert" else f y :: String
--          |              \           \
--          |               \           \
--      zwischen if / then   \           \
--      muss ein Bool-Wert   String      nimmt irgendwas,
--      stehen                           gibt ein String aus,
--                                       da der 'then' Fall auch
--                                       ein String war

--K_a = { x :: Bool, f :: a -> String, y :: a }

-- b)

--K_b |- \y -> \z -> x z (y z) :: (a -> b) -> a -> y

-- y und z ist gebunden -> nicht im Kontext

-- wir wissen durch die Reihenfolge der Argumente und die dazugehörigen Typen,
-- dass y :: (a -> b) und z :: a ist

-- daher können wir schlussfolgern, dass x zwei Argumente nimmt
-- => x :: a -> b -> y
--         |    |     \
--         |    |      \
--         z  (y z)    der Typ der übrig bleibt, wenn wir 'z' und 'y' reinziehen würden

--K_b = { x :: a -> b -> y }


-- c)

--K_c |- \x -> if x then \y -> y x else \z -> 1.0 :: Bool -> (Bool -> Double) -> Double
--        |       |       |___/ /        |
--         \_____/__________|__/         |
--            |             |            |
--         gebunden      gebunden      gebunden (nicht benutzt)
--
-- => alle Variablen sind gebunden, damit ist der Kontext leer!
--
--K_c = { }


-- In der Musterlösung ist eine Typherleitung für jeden Fall noch ausführlich betrachtet