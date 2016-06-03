-- | Aufgabe 7-1 - "Rekursion mit Listen"
--
--

-- a) Implementieren sie eine Funktion die das kleinste Element einer übergebenen Liste ausgibt

-- Diese Variante ist bereits endrekursiv (weil es sich anbietet):

smallest :: Ord a => [a] -> a
smallest []     = error "*** Exception: 7-1.smallest: empty list"
smallest (x:xs) = go x xs
    where 
        go :: Ord a => a -> [a] -> a
        go min []     = min
        go min (y:ys)
            | min <= y  = go min ys
            | otherwise = go y   ys

-- Beispieldurchlauf:

--     smallest [5,2,3]
--  => go 5 (2:[3])
--  =>     | otherwise = go 2 [3]    -- 5 <= 2 ist False, wir springen in den nächsten Fall
--
--  => go 2 (3:[])
--        | 2 <= 3 = go 2 []        -- True
-- 
--  => go 2 [] = 2                  -- wir kommen in den Pattern-Match von der leeren Liste
--  => 2

-- b) Implementieren sie eine Funktion, die eine Zahl an der richtige Stelle einer sortierten Liste einordnet

insert :: Ord a => a -> [a] -> [a]
insert e []     = [e]
insert e (x:xs)
    | e <= x    = e : x : xs
    | otherwise = x : insert e xs

-- Beispieldurchlauf:

--     insert 3 [1,2,5]
-- =>  insert 3 (1:[2,5])
--         | otherwise = 1 : insert 3 [2,5]    -- 3 <= 1 ist False
-- 
-- =>  1 : insert 3 (2:[5])
--         | otherwise = 2 : insert 3 [5]      -- 3 <= 2 ist False
-- 
-- =>  1 : 2 : insert 3 (5:[])
--         | 3 <= 5 = 3 : 5 : []
-- 
-- =>  1 : 2 : 3 : 5 : []
-- =>  [1,2,3,5]

-- Endrekursive Variante (etwas unschön, aber wer was besseres hat -> pull request):
insert' :: Int -> [Int] -> [Int]
insert' e [] = [e]
insert' e l  = go l False []
    where
        go :: [Int] -> Bool -> [Int] -> [Int]
        go []     True  acc = acc
        go []     False acc = go [] True (acc ++ [e])
        go (y:ys) False acc
            | e < y     = go [] True  (acc ++ e : y : ys)
            | otherwise = go ys False (acc ++ [y])

-- c) Implementieren sie eine Funktion, welche das erste Vorkommen eines Elements löscht

remove :: Eq a => a -> [a] -> [a]
remove e []     = []
remove e (x:xs)
    | e == x    = xs
    | otherwise = x : remove e xs

-- Beispieldurchlauf:

--     remove 3 [1,2,3,5,3]
-- 
-- =>  remove 3 (1:[2,3,5,3])
--         | otherwise = 1 : remove 3 [2,3,5,3]  -- 3 == 1 ist False, deswegen kommen wir in den nächsten Fall
-- 
-- =>  1 : remove 3 (2:[3,5,3])
--         | otherwise = 2 : remove 3 [3,5,3]    -- 3 === 2 ist False, deswegen kommen wir in den nächsten Fall
-- 
-- =>  1 : 2 : remove 3 (3:[5,3])
--         | 3 == 3 = [5,3]
-- 
-- =>  1 : 2 : [5, 3]
-- =>  [1, 2, 5, 3]

-- Endrekursive Variante:
remove' :: Int -> [Int] -> [Int]
remove' e [] = []
remove' e l  = go l []
    where
        go :: [Int] -> [Int] -> [Int]
        go []     acc   = acc
        go (x:xs) acc
            | e == x    = go [] (acc ++ xs)
            | otherwise = go xs (acc ++ [x])


-- d) Definieren sie eine Funktion, welche eine Liste vom Typ [Int] sortiert

-- sort :: Ord a => [a] -> [a]
sort :: [Int] -> [Int]
sort [] = []
sort xs = y : sort (remove y xs)
    where y = smallest xs

-- sort' :: Ord a => [a] -> [a]
sort' :: [Int] -> [Int]
sort' []     = []
sort' (x:xs) = insert x (sort' xs)