module Main where

-- | H5-1

length' :: [a] -> Int
length' l = foldl (\xs _ -> succ xs) 0 l

-- point-free
length'' :: [a] -> Int
length'' = foldl (\xs _ -> succ xs) 0

length''' :: [a] -> Int
length''' = foldl (\xs _ -> xs + 1) 0

length'''' :: [a] -> Int
length'''' = foldl (const . succ) 0

-- const :: a -> b -> a
-- const x _ = x

length''''' :: [a] -> Int
length''''' = foldl (\xs x -> const (succ xs) x) 0

-- nur zur Wiederholung!
--length'''''' :: [a] -> Int
--length'''''' l = sum [1 | _ <- l]


reverse' :: [a] -> [a]
reverse' l = foldl (\xs x -> x : xs) [] l

-- point-free
reverse'' :: [a] -> [a]
reverse'' = foldl (\xs x -> x : xs) []

reverse''' :: [a] -> [a]
reverse''' = foldr (\x xs -> xs ++ [x]) []


-- flip vertauscht die Argumente von der Funktion
-- Bsp: flip (/) 2 1 => 0.5
--           (/) 2 1 => 2.0
reverse'''' :: [a] -> [a]
reverse'''' = foldl (flip (:)) []

reverse''''' :: [a] -> [a]
reverse''''' = foldl (\xs x -> flip (:) xs x) []
--                          ->      (:) x xs
--                          ->       x  : xs


-- | H5-3

--  gegeben:

--   baz :: [Int] -> Int
--   baz (x1:x2:xs)        --   a)                    b)
--     | even x1, even x2 = baz (2*x1: xs) + baz (xs ++  [2*x2])
--     | otherwise        = baz (x1 +x2 `div` 2 : 77 : xs)   -- c)
--   baz [x1]             = x1
--   baz []               = 42 `div` 0

--   - | l1 ++ l2 | = | l1 | + | l2 |    1)

--  zu zeigen:
--     - Funktion baz terminiert für,
--     - NICHTLEEREN Listen
--     - NUR für gerade Zahlen

-- AUF)

--   Führen die rekursiven Aufrufe aus der definierten Menge raus?

--   Wir haben drei rekursive Aufrufe:

--   a) 2* x1 ist immer gerade -> wir bleiben in der definierten Menge
--   b) xs enthält nur gerade Zahlen, 2*x2 ist immer gerade -> wir bleiben in der definierten Menge
--   c) Passiert nicht, da wir nur gerade Elemente in der Liste haben -> ist uns also egal

-- DEF)

--   Werden alle möglichen Argumente für unsere Menge (Listen mit nur geraden Elementen) abgefangen?

--   Alles wird abgedeckt wegen otherwise abgedeckt - es gibt nur ein Problem mit 'div 0',
--   aber das passiert nicht, da wir uns nur nichtleere Listen als Argumente anschauen!

-- ABST)

--   Sei n aus N  ^= die Länge der Liste l

--   Wir machen die Abstiegsfunktion von n abhängig.

--   Wir schauen uns nur den 'even'-Fall an, da nur er rekrusive Aufrufe für unsere Argumente
--   definiert.

--   Erster Fall:

--   |l| => |x1:x2:xs| => 2 + |xs| => n
--   |2*x1:xs|         => 1 + |xs| => n - 1

--   Zweiter Fall:

--   |l| => |x1:x2:xs| => 2 + |xs| => n

--   |xs ++ [2*x2]| => |xs| + |[2*x2]|   1)
--                  => n - 2 + 1
--                  => n - 1

--   Damit sind beide Fälle abgedeckt und bewiesen!