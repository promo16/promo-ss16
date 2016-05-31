-- | Aufgabe 7-1 - "Rekursion mit Listen"
--
--

-- a) Implementieren sie eine Funktion die das kleinste Element einer übergebenen Liste ausgibt

smallest :: Ord a => [a] -> a
smallest []     = error "*** Exception: 7-1.smallest: empty list"
smallest (x:xs) = go x xs
    where
        go :: Ord a => a -> [a] -> a
        go min []     = min
        go min (y:ys)
            | min <= y  = go min ys
            | otherwise = go y   ys

-- b) Implementieren sie eine Funktion, die eine Zahl an der richtige Stelle einer sortierten Liste einordnet

insert :: Ord a => a -> [a] -> [a]
insert e []     = [e]
insert e (x:xs)
    | e <= x    = e : x : xs
    | otherwise = x : insert e xs

-- c) Implementieren sie eine Funktion, welche das erste Vorkommen einer als Parameter übergebenen Zahl aus einer Liste entfernt

remove :: Eq a => a -> [a] -> [a]
remove e []     = []
remove e (x:xs)
    | e == x    = xs
    | otherwise = x : remove e xs


-- d) Definieren sie eine Funktion, welche eine Liste vom Typ [Int] sortiert

-- sort :: Ord a => [a] -> [a]
sort :: [Int] -> [Int]
sort [] = []
sort xs = y : sort (remove y xs)
    where y = smallest xs

-- sort' :: Ord a => [a] -> [a]
sort' :: [Int] -> [Int]
sort' [] = []
sort' (x:xs) = insert x (sort' xs)
