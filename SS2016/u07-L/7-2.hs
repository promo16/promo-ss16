-- | Aufgabe 7-2 - "Primfaktorzerlegung"
--
-- Def. (wiki): Die Primfaktorzerlegung ist die Darstellung einer natürlichen Zahl n als Produkt von Primzahlen
--

-- (Aufgabe 4-2)
sieve :: Integral a => a -> [a]
sieve n = go [2..n]
    where
        go :: Integral a => [a] -> [a]
        go []     = []
        go (x:xs) = x : go [y | y <- xs, y `mod` x /= 0]


intFac :: Integral a => a -> [a]
intFac m = go m (sieve m)
    where
        go :: Integral a => a -> [a] -> [a]
        go n (x:xs)
            | n `elem` (x:xs) = [n]                        -- ist n eine Primzahl?
            | n `mod` x == 0  = x : go (n `div` x) (x:xs)  -- teilt x n? Dann ist es ein Primfaktor
--                              (I)        (II)    (III)   
--                                                         (I)   Weil es ein Primfaktor fügen wir es zur resultierenden Liste hinzu
--                                                         (II)  Wir teilen mit Ganzzahldivision 'div' um die restlichen Faktoren im nächsten Schritt zu bestimmen
--                                                         (III) Ein Primfaktor kann aus mehreren gleichen Primzahlen bestehen e.g 8 ~ [2,2,2]
--
            | otherwise       = go n xs                    -- Wenn nichts davon zutrifft dann versuchs mit der nächsten Primzahl

{-

    intFac 10
 => go 10 (sieve 10)
 => go 10 [2,3,5,7,9]
    
    => go 10 (2:xs)
          | 10 `mod` 2 == 0 => 2 : go (10 `div` 2) (2:xs)
    => 2 : go 5 (2:xs)
          | 5 `elem` (2:3:5:7:9) = [5]
    => 2 : [5]
    => [2,5]

-}