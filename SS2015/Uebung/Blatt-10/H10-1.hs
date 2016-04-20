module Main where

----------------------------------------------------------------------------
-- Vorlage für Übung H10-1 zur Vorlesung "Programmierung und Modellierung"
-- am 30.06.2015 an der LMU München
-- verfasst von Steffen Jost & Martin Hofmann
--

-- module AVL where

data Tree a = Empty | Node { label   :: a
                           , left    :: Tree a
                           , right   :: Tree a
                           , balance :: Int
                           }

leaf :: a -> Tree a
leaf a = Node { label = a, left = Empty, right = Empty, balance = 0 }

-- tiefe wollen wir nicht verwenden, da ein Aufruf zu teuer ist!
tiefe :: Tree a -> Int
tiefe Empty = 0
tiefe Node {left=l,right=r} = 1 + max (tiefe l) (tiefe r)

mem_BST :: Ord a => a -> Tree a -> Bool
mem_BST _ Empty = False
mem_BST x (Node y l r _) | x==y      = True
                         | x< y      = mem_BST x l
                         | otherwise = mem_BST x r

search_BST :: Ord a => a -> Tree (a,b) -> Maybe b
search_BST _ Empty = Nothing
search_BST x (Node (k,v) l r _) | x==k      = Just v
                                | x< k      = search_BST x l
                                | otherwise = search_BST x r

{- Rotation werden nicht als eigenständige Funktionen benötigt:
rot_right :: Tree a -> Tree a
rot_right (Node x (Node y r s by) t bx)
  = Node y r (Node x s t (bx+1)) (by+1)

rot_left :: Tree a -> Tree a
rot_left  (Node y r (Node x s t bx) by)
  = Node x (Node y r s (by-1)) t (bx-1)
-}

ins_BST :: Ord a => a -> Tree a -> Tree a
ins_BST x t = fst $ ins_aux  x t
  where
    ins_aux :: Ord a => a -> Tree a -> (Tree a, Int)
    ins_aux e Empty = (leaf e, 1)
    ins_aux e n@(Node nx tl tr b)
      | e == nx = (n,0)
      | e <  nx = -- BST: links einfügen
        let (l1,hdiff) = ins_aux e tl
        in case Node nx l1 tr (b-hdiff) of                          -- ACHTUNG: (b-hdiff) nicht mehr in {-1,0,1}
              -- Hinweis: Damit die Bennenung zu den Folien 07-51ff passt,
              --          bauen wir hier zuerst den Knoten zusammen und matchen diesen gleich wieder.
              n1@(Node _ _ _ b1) | -2<b1, b1<2 ->                -- Balance ist akzeptabel
                  (n1,if hdiff==1 && b == 0 then 1 else  0)

              Node x (Node y r s 0) t (-2) ->                    -- Fall 1,  Folie 07-51
                  (Node  y r (Node x s t (-1)) 1, 1)

              Node x (Node y r s (-1)) t (-2) ->                 -- Fall 2,  Folie 07-52
                  (Node  y r (Node x s t 0) 0, 0)

              Node x (Node y r (Node z u v 0) 1) t (-2) ->       -- Fall 3a, Folie 07-53
                  (Node z (Node y r u 0) (Node x v t 0) 0, 0)

              Node x (Node y r (Node z u v 1) 1) t (-2) ->       -- Fall 3b, Folie 07-53
                  (Node z (Node y r u (-1)) (Node x v t 0) 0, 0)

              Node x (Node y r (Node z u v (-1)) 1) t (-2) ->    -- Fall 3c, Folie 07-53
                  (Node z (Node y r u 0) (Node x v t 1) 0, 0)

      | e >  nx = -- BST: rechts einfügen
        let (r1, hdiff) = ins_aux e tr
        in case Node nx tl r1 (b-hdiff) of                           -- analog zu Links einfügen

              n1@(Node _ _ _ b1) | -2<b1, b1<2 ->                    -- Balance ist akzeptabel
                  (n1, if hdiff==1 && b == 0 then 1 else  0)

-- 1 ^= label
-- 2 ^= left node
-- 3 ^= right node
-- 4 ^= balance
-- 5 ^= right node's label
-- 6 ^= right node's left node
-- 7 ^= right node's right node
-- 8 ^= right node's balance
-- 9 ^= right node's right node's balance

-- for better understanding format at every mapping, so Nodes align


--                  1   _______2________    ________________3_______________   _4_
--                 | | |                |  |                                | |   |
--                 | | |                |  |      5   6   _____7________   8| |   |
--                 | | |                |  |     | | | | |           _9_| | | |   |
--                 | | |                |  |     | | | | |          |   | | | |   |
--                 | | |                |  |     | | | | |          |   | | | |   |
              Node  x  t                   (Node  y   r                s   0)    2 ->           -- wir vergleichen 8) und 4)
{- mapp. 1 -}   ((Node  y  (Node x t s (-1))   r                                   (-1)), 1)

              Node  x  t                   (Node  y   r                s   1)    2 ->           -- wir vergleichen 8) und 4)
{- mapp. 2 -}   ((Node  y  (Node x t s    0)   r                                     0 ), 0)

              Node  x  t                   (Node  y   r  (Node z u v   0)  1)  (-2) ->          -- wir gehen alle Möglichenkeiten von 9) durch => 0
{- mapp. 3 -}    ((Node  z (Node x t v     0)   (Node  y   u                v   0)    0 ), 0)

              Node  x  t                   (Node  y   r  (Node z u v   1)  1)  (-2) ->          -- wir gehen alle Möglichenkeiten von 9) durch => 1
{- mapp. 4 -}      ((Node  z (Node x t v  (-1))   (Node  y   u                v   0)    0 ), 0)

              Node  x  t                   (Node  y   r  (Node z u v (-1)) 1)  (-2) ->          -- wir gehen alle Möglichenkeiten von 9) durch => -1
{- mapp. 5 -}    ((Node  z (Node x t v     0)   (Node  y   u                v   1)    0 ), 0)

----------------------------------------------------------------
-- Funktionen zum Testen des Codes

-- Erzeugt einen Baum aus einer Liste von Werten
-- genTree :: Ord a => [a] -> Tree a
genTree xs = foldr ins_BST Empty xs

-- Überprüft, ob ein Tree wirklich ein AVL-Baum ist
-- Auch diese Funktionen wollen wir nie verwenden, außer zum testen.
isAVL :: Tree a -> Bool
isAVL = snd . isAVLaux
  where -- die teure Höhenberechnung ist integriert, nicht jedes Mal separat!
    isAVLaux :: Tree a -> (Int, Bool)
    isAVLaux Empty = (0,True)
    isAVLaux Node {left=l,right=r,balance=b} =
      let (hl,avll) = isAVLaux l
          (hr,avlr) = isAVLaux r
          height    = 1 + max hl hr
          diff      = hr - hl
      in (height, avll && avlr && b==diff && (abs diff) <= 1)

-- QuickCheck Property für schnelle Tests.
prop_AVL :: [Int] -> Bool
prop_AVL = isAVL . genTree
--
-- > Test.QuickCheck.quickCheck prop_AVL



-------------------------------------------------------------------
-- Show Instanz zum einfach Testen

instance (Show a) => Show (Tree a) where
  show = printTree []

newline :: String
newline = "\n"

data Pfad = Links | Rechts

printTree :: (Show a) => [Pfad] -> Tree a -> String
printTree _      Empty = ""
printTree indent t
  =    printTree (           indent ++ [Rechts]) (right t)
    ++ printIndent indent ++ show (label t) ++ (concat $ replicate (7 - (length indent)) "   ") ++" B(" ++ show (balance t) ++ ")" ++ newline
    ++ printTree (flipLast   indent ++ [Links ]) (left t)

flipLast :: [Pfad] -> [Pfad]
flipLast []       = []
flipLast [Rechts] = [Links ]
flipLast [Links ] = [Rechts]
flipLast (h:t)    = h : flipLast t

printIndent :: [Pfad] -> String
printIndent [ ]        = ""
printIndent [Rechts]   = " /="
printIndent [Links ]   = " \\="
printIndent (Rechts:p) = "   " ++ printIndent p
printIndent (Links :p) = "|  " ++ printIndent p