-- 5-1 Auswertung mit Substitutionsmodell --

-- Einführung - Lambdaschreibweise:

-- Die folgenden Ausdrücke sind equivalent zueinander:

foo :: Int -> Int -> Int -> Int
foo    x y z = x + y + z

foo' :: Int -> Int -> Int -> Int
foo'   x y   =  \z     -> x + y + z

foo'' :: Int -> Int -> Int -> Int
foo''  x     =  \y z   -> x + y + z

foo''' :: Int -> Int -> Int -> Int
foo'''       =  \x y z -> x + y + z

-- --------------------------------------------

goo :: (Int, Char) -> String
goo (i, c) = "hallo " ++ c

goo' :: (Int, Char) -> String
goo'       = \(i,c) -> "hallo " ++ c

goo'' :: (Int, Char) -> String
goo''      = \(_,c) -> "hallo " ++ c

-- --------------------------------------------

hoo :: [a] -> a
hoo (x:xs) = x

hoo'  :: [a] -> a
hoo'       = \(x:xs) -> x

hoo'' :: [a] -> a
hoo''      = \(x:_) -> x

-- --------------------------------------------

ioo  :: a -> Int
ioo  x = 10

ioo' :: a -> Int
ioo' _ = 10

ioo''  :: a -> Int
ioo''  = \x -> 10

ioo''' :: a -> Int
ioo''' = \_ -> 10


-- Der Backslash heißt 'Lambda' und kommt aus dem Lambda-Kalkül, auch Abstraktion genannt,
-- was umgangssprachlich aussagt:
--
--    "Ich erwarte hier ein Argument in irgendeiner Form"
--
-- wobei man diese Form genauso wie bei den Argumenten einer normalen Funktion mit Pattern-Matching festlegen kann.

-- Wir benutzen diese Darstellungsform von Funktionen damit es einfacher wird damit Substitution zu betreiben.

-- Wir unterscheiden zwischen folgenden Ausdrücken:

-- * Funktionen           e.g 'f x = x + 1'
-- * Funktionsanwendungen e.g 'f 1'
-- * Spezielle Sprachkonstrukte - if-then-else und let-in

-- * formaler Parameter   e.g 'x' in 'f x = x * x' also die Parameter in der Funktionsdefinition
-- * aktueller Parameter, e.g '2' in 'f 2'         also die Parameter in der Funktionsanwendung


-- Spezielle Regeln:

-- ########################
-- # if B then A1 else A2 #
-- ########################

-- Wir werten immer ZUERST B aus, und danach erst A1 ODER A2. Egal welche Auswertungsstrategie wir benutzen.

-- ######################
-- # let A = ... [in] B #
-- ######################

-- Wir fügen A der Umgebung hinzu und B wird mit der neuen Umgebung ausgeführt. NACHDEM B ausgewertet wurde,
-- wird A aus der Umgebung entfernt. Ob das 'in' dasteht oder nicht, ist egal. Alles was nach dem Ende vom
-- Ausdruck A kommt, wird mit der aktualisierten Umgebung auswertet.




-- Nun zur Aufgabe:
--
-- gegeben:
--
quadrat :: Num a => a -> a
quadrat = \x -> x * x

{-

            Ausdruck               |            Umgebung          
1.                                 | []                             -- wir fangen immer mit der leeren Umgebung an, ausser es anders angegeben
2. let a = let a = 2 in quadrat a  | [(a, let a = 2 in quadrat a)]  -- der Parameter 'a' enthält den gesamten inneren Ausdruck
3. let a = 2 in quadrat a          | [(a, 2), (a, let a = 2 in quadrat a)] -- der Parameter 'a' enthält die 2
4. quadrat a                       | [(a, 2), (a, let a = 2 in quadrat a)] -- wir suchen das ERSTE a in der Umgebung, von links nach rechts
5. quadrat 2                       | [(a, 2), (a, let a = 2 in quadrat a)] -- wir substitueren quadrat für seine Lambda-Schreibweise
6. (\x -> x * x) 2                 | [(a, 2), (a, let a = 2 in quadrat a)] -- einsetzen in den Lambda-Ausdruck (β - Reduktion)
7. 2 * 2                           | [(a, 2), (a, let a = 2 in quadrat a)] -- weiter auswerten
8. 4                               | [(a, 2), (a, let a = 2 in quadrat a)] -- nun müssen wir nur noch die Umgebung vom ersten 'a' Ausdruck wegschmeißen
9.                                 | [(a, let a = 2 in quadrat a)]         -- da nach dem ersten 'let' auch kein Ausdruck kommt, müssen wir dessen Umgebung auch wegschmei0en
10.                                | []                                    -- und nun sind wir fertig

-}


-- Bemerkungen:
-- 
-- * Die Umgebung wird immer von links nach rechts aufgefüllt und genauso gelesen
-- * Es wird immer das erste Vorkommen einer Variable gesucht, deswegen haben wir vorher in 'quadrat a' nicht den ersten Ausdruck eingesetzt
-- * Die Reihenfolge der Auswertung ist bei reiner Substitution nicht vorgegeben (in den folgenden Aufgaben jedoch schon)
-- * Man muss sich merken welcher Ausdruck zu welchem Teil der Umgebung gehört, deswegen haben wir bei 8 nicht die gesamte Umgebung weggeworfen