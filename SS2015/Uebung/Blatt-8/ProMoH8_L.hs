module Main where



-- (\x -> f x) (\(+1) -> (+1)) (+1)

-- (\x -> f x) (+1)

-- f (+1) :: B -> B

-- f 4



-- | H8-1 Typherleitung

-- Hier wurde der Kontext mit Absicht nicht gekürzt um die Unterschiede zu zeigen!

-- 1. { f :: (A -> A) -> B -> B } |- (\x -> f x) (\y -> y) :: B -> B      (App 2, 3)

-- 2. { f :: (A -> A) -> B -> B } |- \x -> f x :: (A -> A) -> B -> B      (Abs 4)

-- 3. { f :: (A -> A) -> B -> B } |- \y -> y :: A -> A                    (Abs 5)

-- 4. { f :: (A -> A) -> B -> B, x :: A -> A } |- f x :: B -> B           (App 6, 7)

-- 5. { f :: (A -> A) -> B -> B, y :: A } |- y :: A                       (Var)

-- 6. { f :: (A -> A) -> B -> B, x :: A -> A } |- f :: (A -> A) -> B -> B (Var)

-- 7. { f :: (A -> A) -> B -> B, x :: A -> A } |- x :: A -> A             (Var)


-- -- | H8-2 Unifikation

-- -- a)

-- Sei unify := u

-- Implizite Rechtsklammerung nicht vergessen!

--     u { Int -> A = B -> (Bool -> B), Y -> Int = A }

--  => u { Int = B, A = Bool -> B, Y -> Int = A }

--  => [Int/B] u { A = Bool -> Int, Y -> Int = A }

--  => [Int/B, (Bool -> Int)/A] u { Y -> Int = Bool -> Int }

--  => [Int/B, (Bool -> Int)/A] u { Y = Bool, Int = Int }

--  => [Int/B, (Bool -> Int)/A, Bool/Y] u {}

--  => [Int/B, (Bool -> Int)/A, Bool/Y] ist der allgemeinste Unifikator


-- -- b) falscher Unifikator wo alle Typen konkret sind!

--     u { A -> B = D -> Y, B -> D = O -> N, N = A }

--  => u { A = D, B = Y, B = O, D = N, N = A }

--  => u { A = D = N, B = Y = O }

--  z.B [Int/A, Double/D, Int/N, Char/B, Char/Y, String/O]

--  -- c) nicht allgemeinste Unifikator

--     u { N -> B = D -> O, A = N, A -> B = D -> Y }

--  => u { N = D, B = O, A = N, A = D, B = Y}

--  => u { N = D = A, B = O = Y }

--  => [N/A, N/D, B/O, B/Y] u { N = N = N, B = B = B }

--  => [N/A, N/D, B/O, B/Y] u {}

--  => [N/A, N/D, B/O, B/Y] ist der allgemeinste Unifikator

--  wir machen einen Typ konkret - z.B 'N'

--  damit fügen wir eine Substitution von N nach Int hinzu

--  und aktualisieren alle bisherigen Substitutionen

--  => [Int/N, Int/A, Int/D, B/O, B/Y] ist ein nicht allgemeinster Unifikator

--  => [Int/N, N/A, N/D, B/O, B/Y] ist ein nicht allgemeinster Unifikator
