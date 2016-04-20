{-
  "Programmierung und Modellierung", LMU München, Sommersemester 2015, Lehrstuhl TCS

  Vorlage Aufgabe H6-?TODO?

  Bitte nichts verändern was oberhalb von "NUR AB HIER BEARBEITEN"
  steht!

  Zuerst zeigen wir als Beispiel die Lösung zu der analogen Aufgabe
  mit unseren selbst-definierten Listentyp.
-}

newline :: String
newline = "\n"

data List a = Leer | Element a (List a)

instance (Show a) => Show (List a) where
  show = printList

printList :: (Show a) => List a -> String
printList l = printLaux 0 l
  where
    printLaux n Leer = replicate n ' ' ++ "[]"
    printLaux n (Element h t)
      = replicate n ' ' ++ show h ++ ":"
        ++ newline ++ printLaux (n+1) t

{-
Verwenden Sie "putStrLn" falls GHCI das Zeichen
zum Zeilenvorschub bei der Ausgabe in GHCI nicht ausführt:

*Main> Element 1 $ Element 2 $ Element 3 $ Leer
1:
 2:
  3:
   []

*Main> printList $ Element 1 $ Element 2 $ Element 3 $ Leer
"1:\n 2:\n  3:\n   []"

*Main> putStrLn $ printList $ Element 1 $ Element 2 $ Element 3 $ Leer
1:
 2:
  3:
   []

-}


-- Datentypdeklaration für Bäume:

data Tree a = Empty
            | Node { label :: a, left,right :: Tree a }

leaf :: a -> Tree a
leaf a = Node a Empty Empty


-- Ein paar Bäume zum Testen

t1 = Node 6 (Node 3 (leaf 2) (Node 8 (leaf 5) Empty))
            (Node 8 Empty (leaf 4))

t0 = Node 1 (Node 2 (leaf 3) (leaf 4))
            (Node 5 (leaf 6) (leaf 7))

t2 = Node 1 (Node 2 (Node 3 (leaf 3) Empty) Empty) Empty

t3 = Node 1 Empty (Node 2 Empty (Node 3 Empty (leaf 4)))

t4 = Node 1 (Node 2 (Node 3 (leaf 4) Empty) Empty)
            (Node 5 Empty (Node 6 Empty (leaf 7)))

t5 = Node 1 (Node 2 (Node 3 Empty (leaf 4)) Empty)
            (Node 5 Empty (Node 6 (leaf 7) Empty))

t8 = Node 69 (leaf 77) (Node 44 (leaf 64) (leaf 7))

t9 = Node 6 (Node 3 (leaf 2) (Node 4 (leaf 5) Empty))
            (Node 7 Empty (Node 9 (leaf 8) Empty))

-----------------------------------------
---- !!!! NUR AB HIER BEARBEITEN !!! ----
-----------------------------------------

tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)


-- Vielleicht nützlich?!
data Pfad = Links | Rechts

printTree :: (Show a) => [Pfad] -> Tree a -> String
printTree _      Empty = ""
printTree indent t = printTree (indent ++ [Rechts]) (right t)
       ++ newline ++ printIndent indent ++ show (label t)
                  ++ printTree (flipLast   indent ++ [Links ]) (left t)


-- | Without record syntax

--printTree :: (Show a) => [Pfad] -> Tree a -> String
--printTree _      Empty        = ""
--printTree indent (Node x l r) = printTree (indent ++ [Rechts]) r
--       ++ newline ++ printIndent indent ++ show x
--                             ++ printTree (flipLast   indent ++ [Links ]) l

flipLast :: [Pfad] -> [Pfad]
flipLast []       = []
flipLast [Rechts] = [Links ]
flipLast [Links ] = [Rechts]
flipLast (h:t)    = h : flipLast t


printIndent :: [Pfad] -> String
printIndent [ ]        = ""
printIndent [Rechts]   = " /-"
printIndent [Links ]   = " \\-"
printIndent (Rechts:p) = "   " ++ printIndent p
printIndent (Links :p) = "|  " ++ printIndent p


helper :: Show a => Tree a -> IO()
helper tree = putStrLn $ printTree [] tree