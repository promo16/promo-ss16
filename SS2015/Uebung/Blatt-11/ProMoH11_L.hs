module Main where

import Data.List (subsequences, sortBy)
import Data.Function (on)

-- | H11-1 - Auswertungsstrategien

-- a)




-- (\x -> \y -> y ( y x )) 9 ((\a b -> b + b) (7*(2+3)))

-- Call-by-Name

--     (\x -> \y -> y ( y x )) 9 ((\a b -> b + b) (7*(2+3)))
--
-- =>  (\y -> y ( y 9 )) ((\a b -> b + b) (7*(2+3)))
--
-- =>  ((\a b -> b + b) (7 * (2 + 3))) (((\a b -> b + b) (7 * (2 + 3))) 9)
--
-- =>  (\b -> b + b) (((\a b -> b + b) (7 * (2 + 3))) 9)
--
-- =>  (((\a b -> b + b) (7 * (2 + 3))) 9) + (((\a b -> b + b) (7 * (2 + 3))) 9)
--
-- =>  ((\b -> b + b) 9) + (((\a b -> b + b) (7 * (2 + 3))) 9)
--
-- =>  ((\b -> b + b) 9) + ((\b -> b + b) 9)
--
-- =>  (9 + 9) + ((\b -> b + b) 9)
--
-- =>  (9 + 9) + (9 + 9)
--
-- =>  18 + (9 + 9)
--
-- =>  18 + 18
--
-- =>  36


-- Call-by-Value




--     (\x -> \y -> y ( y x )) 9 ((\a b -> b + b) (7*(2+3)))
--
--
-- =>  (\x -> \y -> y ( y x )) 9 ((\a b -> b + b) (7*5)
--
-- =>  (\x -> \y -> y ( y x )) 9 ((\a b -> b + b) 35)
--
-- =>  (\x -> \y -> y ( y x )) 9 (\b -> b + b)
--
-- =>  (\y -> y ( y 9 )) (\b -> b + b)
--
-- =>  (\b -> b + b) ( (\b -> b + b) 9 )
--
-- =>  (\b -> b + b) (9 + 9)
--
-- =>  (\b -> b + b) 18
--
-- =>  18 + 18
--
-- =>  36


-- | H11-2 - Hasse-Diagramm


-- Sei P die Potenzmenge
-- Sei M := {1, 2, 3, 4}
-- Sei R := Teilmenge

-- Hasse-Diagramm zu der geordneten Menge ( P(M), R )

-- Überprüfung der Relation (keine echten Beweise - nur Beispiele):

-- Transitivität: {1} R {1, 2} && {1,2} R {1,2,3} <=> {1} R {1, 2, 3}
-- Antisymmetrie: {4,5} R {5,4} && {5,4} R {4,5} <=> {4, 5} = {5, 4}
-- Reflexivität : {6,7,8} R {6,7,8}

-- Was ist die Potenzmenge von {1,2,3,4}

potenzmenge = sortBy (compare `on` length) $ subsequences [1,2,3,4]

-- [[],     [1],    [2],    [3],    [4],
--  [1,2],  [1,3],  [2,3],  [1,4],  [2,4], [3,4],
--  [1,2,3],[1,2,4],[1,3,4],[2,3,4],[1,2,3,4]]

-- Wir fangen mit der leeren Menge an, weil sie immer Teilmenge aller anderen Mengen ist

-- c.b.a das darzustellen => Tafelanschrieb


-- | H11-3

-- Determiniertheit: Gleiche Eingabe => Gleiche Ausgabe

-- Determinismus: Intern wird die Reihenfolge der Prozesszustände immer gleich durchlaufen

-- Nebenläufigkeit: Berechnungen die 'echt' simultan passieren und evtl. miteinander interagieren

-- Parallelität: Eine Berechnung die in Teile aufgespalten werden kann und gleichzeitig
--               berechnet wird, die dann am Ende wieder zusammengefügt werden

-- b) Die 'Par'-Monade wurde so definiert, dass sie ausschließlich Parallelität benutzt
--    und keine Nebenläufigkeit! Das heißt es kommt immer ein deterministisches Ergebnis raus!
--    Deswegen kann man nicht auf 'irgendeine' Lösung warten.