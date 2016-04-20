{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
{-
  "Programmierung und Modellierung", LMU München, Sommersemester 2015, Lehrstuhl TCS

  Vorlage Aufgabe H6-3

  Bitte nichts verändern was oberhalb von "NUR AB HIER BEARBEITEN"
  steht! Also nur ab Zeile 155 editieren!

  Zum Kompilieren muss die Bibliothek "diagrams" installiert sein.
  Dazu bitte in einer Textkonsole folgende 2 Befehle ausühren:
      > cabal update
      > cabal install diagrams
  Das Tool cabal-install ist in der Haskell-Plattform enthalten;
  ggf. separat installieren.
  Falls Sie Probleme haben, so verwenden Sie einfach die Rechner
  am CIP-Pool der Informatik; dies ist auch aus der Ferne möglich.
  Informationen zum Remote-Login am CIP-Pool finden Sie auf
      http://www.rz.ifi.lmu.de/FAQ/index.html
  im Abschnitt "Von zu Hause/remote aus ..."

  Ein Diagramm wird durch Ausführen dieser Datei erstellt:
      > runghc H6-3.hs -o MyOutput.svg -h 600 -S Triangle
  Parameter:
      -o [Dateiname]    legt Name der Ausgabedatei fest.
      -h [Pixel]        legt Höhe des Ausgabe fest.
      -S [String]       legt das auszugebende Diagramm fest.
  Die Liste der möglichen Diagramme erhalten Sie mit Option -L
  oder auch hier ab Zeile 40-55.

  Bitte nur ab Zeile 155 editieren!
-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

type Diag = Diagram B


main = mainWith [("Triangle"    ,diagTriangle)
                ,("Composition" ,diagComposition)
                ,("Tree0"       ,diagTree t0)
                ,("Tree1"       ,diagTree t1)
                ,("Tree2"       ,diagTree t2)
                ,("Tree3"       ,diagTree t3)
                ,("Tree4"       ,diagTree t4)
                ,("Tree5"       ,diagTree t5)
                ,("Tree6"       ,diagTree t6)
                ,("Tree7"       ,diagTree t7)
                ,("Tree8"       ,diagTree t8)
                ,("Tree9"       ,diagTree t9)
                ]


--------------------------------------------
---- Erstmal die Bäume, wie wir sie kennen:

data Tree a = Empty
            | Node { label :: a, left,right :: Tree a }

leaf :: a -> Tree a
leaf a = Node a Empty Empty

-- Ein paar Bäume zum Testen:

t0 = Node 1 (Node 2 (leaf 3) (leaf 4))
            (Node 5 (leaf 6) (leaf 7))

t1 = Node 6 (Node 3 (leaf 2) (Node 8 (leaf 5) Empty))
            (Node 8 Empty (leaf 4))

t2 = Node 'A' (Node 'B' (Node 'C' (leaf 'D') Empty) Empty) Empty

t3 = Node 1 Empty (Node 2 Empty (Node 3 Empty (leaf 4)))

t4 = Node 1 (Node 2 (Node 3 (leaf 4) Empty) Empty)
            (Node 5 Empty (Node 6 Empty (leaf 7)))

t5 = Node (0,0) (Node (1,0) (Node (2,0) Empty (leaf (2,1))) Empty)
                (Node (0,1) Empty (Node (0,2) (leaf (1,2)) Empty))

t6 = Node "Root" (Node "L" (Node "LL" (leaf "LLL")
                                      (Node "LLR" (Node "LLRL" (leaf "LLRLL") Empty) (leaf "LLRR"))
                            ) Empty)
                 (Node "R" (Node "RL" (leaf "RLL") (Node "RLR" (leaf "RLRL") Empty))
                           (leaf "RR")
                 )

t7 = Node 1.2 (Node 2.3 (Node  3.4  (leaf  4.5)  (leaf  5.6 )) (Node  6.7  (leaf  7.7)  (leaf  8.9 )))
              (Node 9.1 (Node 10.11 (leaf 11.12) (leaf 12.13)) (Node 13.14 (leaf 14.15) (leaf 15.16)))

t8 = Node 69 (leaf 77) (Node 44 (leaf 64) (leaf 7))

t9 = Node 6 (Node 3 (leaf 2) (Node 4 (leaf 5) Empty))
            (Node 7 Empty (Node 9 (leaf 8) Empty))

-------------------------------------------------


-------------------------------------------------
-- Beispiel:
--   Diagramm das einen Text in einem Kreis zeichnet
--
--   In Erweiterung des Codes auf dem Übunbgsblatt:
--     * "pad 1.5"   addiert etwas Abstand um alles herum
--     * "# fc blue" Vordergrundfarbe blau für den Text
--     * "# fontSizeL sz" macht lange Texte etwas kleiner

diagNode :: String -> Diag
diagNode s = pad 1.5 $ t `atop` c
  where
    c    = circle 1
    t    = text s # fc blue
                  # fontSizeL sz
    sz   = min 0.6 (3/fromIntegral (length s))



-------------------------------------------------
-- Beispiel:
--   einfaches Diagramm eines Baumes mit 2 Blättern

diagTriangle :: Diag
diagTriangle = connectOutside "X" "L" $
               connectOutside "X" "R" $
          nx
          ===
      (nl ||| nr) # center
  where
    nx = named "X" ( diagNode "Root" )  -- Hier drei
    nl = named "L" $ diagNode "Left"    -- äquivalente Schreibweisen
    nr = diagNode "Right" # named "R"   -- zur Demonstration der Notationen



-------------------------------------------------
-- Beispiel: Mehrere Diagramme zusammenfügen
--   Nur ein Beispiel, für Ihre Lösung evtl. nutzlos

diagComposition :: Diag
diagComposition = diagTriComp diagTriangle diagTriangle diagTriangle

diagTriComp :: Diag -> Diag -> Diag -> Diag
diagTriComp nx nl nr = (
       nx # named "XX"
       ===
      (nl # named "LL" ||| nr # named "RR") # center
  ) # connectOutside "XX" "LL"
    # connectOutside "XX" "RR"



-----------------------------------------
---- !!!! NUR AB HIER BEARBEITEN !!! ----
-----------------------------------------



diagTree :: Show s => Tree s -> Diag
diagTree tree = diagTAux "Z" tree
  where diagTAux _ Empty = diagNode "Empty"         -- wir zeigen den 'Empty' Knoten an!
        diagTAux nRoot t = let nLeft  = 'L':nRoot   -- neuer Name für den linken  Teilbaum wird generiert
                               nRight = 'R':nRoot   -- neuer Name für den rechten Teilbaum wird generiert
                               dRoot  = diagNode (show $ label t) # named nRoot  -- das label wird als Text gesetzt
                               dLeft  = diagTAux nLeft  (left  t) # named nLeft  -- wir holen und rekursiv den linken
                               dRight = diagTAux nRight (right t) # named nRight -- und rechten Teilbaum
                           in  connectOutside nRoot nLeft  $          -- wir verbinden (wie im Beispiel) die beiden Knoten
                               connectOutside nRoot nRight $          -- mit dem Elternknoten
                               dRoot === (dLeft ||| dRight) # center  -- wir plazieren beide Knoten unter den Eltern und setzen
                                                                      -- den linken links vom rechten

-- analog die Pattern-Matching Schreibweise mit 'where'´s
diagTree' :: Show s => Tree s -> Diag
diagTree' tree = diagTAux "Z" tree
  where diagTAux _ Empty = diagNode "Empty"         -- wir zeigen den 'Empty' Knoten an!

        diagTAux nRoot (Node x l r) = connectOutside nRoot nLeft  $          -- wir verbinden (wie im Beispiel) die beiden Knoten
                                      connectOutside nRoot nRight $          -- mit dem Elternknoten
                                      dRoot === (dLeft ||| dRight) # center  -- wir plazieren beide Knoten unter den Eltern und setzen
                                                                             -- den linken links vom rechten
           where nLeft  = 'L' : nRoot   -- neuer Name für den linken  Teilbaum wird generiert
                 nRight = 'R' : nRoot   -- neuer Name für den rechten Teilbaum wird generiert
                 dRoot  = diagNode (show x) # named nRoot  -- das label wird als Text gesetzt
                 dLeft  = diagTAux nLeft  l # named nLeft  -- wir holen und rekursiv den linken
                 dRight = diagTAux nRight r # named nRight -- und den rechten Teilbaum
