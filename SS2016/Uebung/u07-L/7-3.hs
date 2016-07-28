-- | Aufgabe 7-3 - "Arithmetische Operationen ohne die Standartbibliothek"
--
-- gegeben:

succ' :: Integer -> Integer
succ' x = x + 1

pred' :: Integer -> Integer
pred' x = x - 1

-- a) Implementieren sie 'plus' und 'minus'

plus :: Integer -> Integer -> Integer
plus n m
    | n == 0    = m                             -- plus  0 3 => 3
    | n  < 0    = plus (succ' n) (pred' m)      -- plus -1 3 => plus (succ' -1) (pred' 3) => plus 0 2 => 2
    | otherwise = plus (pred' n) (succ' m)      -- plus  1 3 => plus (pred'  1) (succ' 3) => plus 0 4 => 4

minus :: Integer -> Integer -> Integer
minus n m
    | m == 0    = n                             -- minus 3  0 => 3
    | m  < 0    = minus (succ' n) (succ' m)     -- minus 3 (-1) => minus (succ' 3) (succ' -1) => minus 4 0 => 4
    | otherwise = minus (pred' n) (pred' m)     -- minus 3  1 => minus (pred' 3) (pred'  1) => minus 2 0 => 2


-- b) Implementieren sie 'mult'

mult :: Integer -> Integer -> Integer
mult n m
    | m == 0    = 0                             -- mult 3  0 => 0
    | m  < 0    = mult n (succ' m) `minus` n    -- mult 3 -1 => mult 3 (succ' -1) `minus` 3 => mult 3 0 `minus` 3 => 0 `minus` 3 = -3
    | otherwise = mult n (pred' m) `plus`  n    -- mult 3  1 => mult 3 (pred'  1) `plus`  3 => mult 3 0 `plus`  3 => 0 `plus`  3 =  3

-- c) Implementieren sie 'mod' (nur natürliche Zahlen)

mod' :: Integer -> Integer -> Integer
mod' n m
  | n < m     = n
  | otherwise = mod' (n `minus` m)  m