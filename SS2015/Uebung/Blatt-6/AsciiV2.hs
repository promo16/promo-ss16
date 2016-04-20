module Main where

data Tree a = Empty | Node { label :: a, left, right :: Tree a }


-- at every right branch we have to memorize where to put our '|'
-- left branches don't have to memorize anything
--
--                     depth
--                      \_/
--                       |
--                       |
data ParentNode = RNode Int | LNode

leaf :: a -> Tree a
leaf a = Node a Empty Empty


-- main entry point
printTree :: Show a => Tree a -> String
printTree tree = acc tree 0 []
--                                depth  previous nodes
--                                  |          |
--                                  |          |
  where acc :: Show a => Tree a -> Int -> [ParentNode] -> String
        acc Empty        _     _           = []
        acc (Node x l r) depth pnodes = show x ++ "\n"
                                     ++ childR pnodes r ++ acc r (depth + 1) (pnodes ++ [(RNode (depth+1))])
                                     ++ childL pnodes l ++ acc l (depth + 1) (pnodes ++ [LNode])

childL :: [ParentNode] -> Tree a -> String
childL _ Empty = []
childL pnodes _ = printParents pnodes ++ "\\-"

childR :: [ParentNode] -> Tree a -> String
childR _ Empty  = []
childR pnodes _ = printParents pnodes ++ "|-"

printParents :: [ParentNode] -> String
printParents []             = []
printParents (LNode    :xs) = " " ++ printParents xs
printParents ((RNode n):xs) = '|' : replicate n ' ' ++ printParents xs

helper :: (Show a) => Tree a -> IO()
helper t = putStrLn $ printTree t


main :: IO ()
main = helper t1

-- Examples

t0 = Node 1 (Node 2 (leaf 3) (leaf 4))
            (Node 5 (leaf 6) (leaf 7))

t1 = Node 6 (Node 3 (leaf 2) (Node 8 (leaf 5) Empty))
            (Node 8 Empty (leaf 4))


t2 = Node 1 (Node 2 (Node 3 (leaf 3)
                              Empty)
                     Empty)
             Empty

t3 = Node 1 Empty (Node 2 Empty (Node 3 Empty (leaf 4)))

t4 = Node 1 (Node 2 (Node 3 (leaf 4) Empty) Empty)
            (Node 5 Empty (Node 6 Empty (leaf 7)))

t5 = Node 1 (Node 2 (Node 3 Empty (leaf 4)) Empty)
            (Node 5 Empty (Node 6 (leaf 7) Empty))

t8 = Node 69 (leaf 77) (Node 44 (leaf 64) (leaf 7))

t9 = Node 6 (Node 3 (leaf 2) (Node 4 (leaf 5) Empty))
            (Node 7 Empty (Node 9 (leaf 8) Empty))

tree = Node 6 (Node 3 (leaf 2)
                      (Node 4 (leaf 5)
                               Empty))
              (Node 7 Empty
                      (Node 9 (leaf 8)
                               Empty))