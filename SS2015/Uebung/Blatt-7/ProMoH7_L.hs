module Main where

-- | H7-1


-- a) Alle freien Variablen sind gesucht!

--   i)   (p q) r
--    --> {p, q, r}

--   ii)  (\a -> b a) (\c -> (d c) e)
--    --> {b, d, e}

--   iii) (\x -> x) (\y -> y) (\z -> z)
--    --> {}

--   vi)  (\u -> v) (\v -> u) (\w -> w)
--    --> {v, u}

--   v)   (\f -> (\g -> (\h -> (h f) i))) (g j)
--    --> {g, j, i}

-- b) Es werden nur die freien Variablen ersetzt!

--   i)   (\a -> b a) [x/a, y/b]

--   In Worten - ersetze alle freien Variablen im Term:

--   (\a -> b a)

--   Mit:

--     - Ersetze b für y
--     - Ersetze a mit x
--    --> (\a -> y a)

--   ii)  ((\x -> (\y -> x (y z))) x) [a/x, b/y, (\c -> d)/z]
--    --> ((\x -> (\y -> x (y (\c -> d)))) a)

--   iii) p (q r) [q/r, p/t, s/q]
--    --> p (s s)

--   iv)  (\u -> (\v -> u w)) [(u v)/w]
--    --> (\a -> (\b -> a (u v)))

--    -- Hier lassen wir uns zwei neue Variablennamen (frische Variablen)
--    -- geben, da sonst die gebundenen '\u' und '\v' nicht mehr eindeutig wären


-- | H7-2

-- https://en.wikipedia.org/wiki/Lambda_calculus#Substitution

data Term = Var Char      -- Variable
          | Const Int     -- Konstante
          | App Term Term -- Applikation
          | Abs Char Term -- Abstraktion
  deriving Show
-- a)

freeVars :: Term -> [Char]
freeVars (Var x)     = [x]
freeVars (Const _)   = []
freeVars (App e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Abs x  e1) = filter (/=x) (freeVars e1)

-- Abstraktion bindet eine Variable, deswegen müssen wir sie
-- aus dem Rest des Ergebnisses rausfiltern!


-- b)

genFreshV :: [Char] -> Char
genFreshV vs = head $ filter (\c -> not $ c `elem` vs) ['a'..'z']

subst :: (Char, Term) -> Term -> Term

-- eine Variable ist gefunden und wir schauen ob es die zu substituierende ist
subst (c, t1) (Var c')
  | c == c' = t1
  | otherwise = Var c'

-- wir verändern nur Variablen
subst (c, t1) (Const x)   = Const x

--                                    rekursive Aufrufe
--                                     _______|_______
--                                    /               \
subst (c, t1) (App e1 e2) = App (subst (c, t1) e1) (subst (c, t1) e2)
subst (c, t1) (Abs c' e1)
-- 1)
  | c == c'         = (Abs c' e1)

-- 2)    i)                   ii)                       iii)
--       |                     |                          |
  | c' `elem` fv_t1 =  Abs freshV (subst (c, t1) $ subst (c', Var freshV) e1)

-- 3)
  | otherwise       =  Abs c' (subst (c, t1) e1)
  where
    fv_t1  = freeVars t1 -- alle freien Variablen im Term den wir einsetzen wollen
    fv_e1  = freeVars e1 -- alle freine Variablen im Term den wir verändern wollen
    freshV = genFreshV (fv_t1 ++ fv_e1) -- eine sicher nicht benutzte freie Variable


-- 1)
-- Substitution ersetzt nur frei gebundene Variablen, da dies aber bereits
-- eine gebundene Variable ist müssen wir hier nichts mehr tun.

-- 2)

-- i)

-- Falls die Variable der Abstraktion (in diesem Fall 'c'') in dem
-- Term (t1) vorkommt, müssen wir darauf achten ihr einen neuen Namen zu
-- geben, damit es zu keinen Überlappungen kommt.


-- ii)

-- Wir überschreiben den vorherigen Namen der Variable mit einem neuen und
-- nicht benutzen Namen. Dann rufen wir uns rekursiv auf den folgenden Term auf
-- um weiterhin alle folgenden Variablen zu substituieren.


-- iii)

-- Damit wir sicher gehen können, dass die alte freie Variable einen neuen Namen bekommt
-- rufen wir rekursiv widerrum die Substitution auf mit den Argumenten (c', Var freshV) -
-- das bedeutet wir ersetzen den alten freien Variablennamen mit einem neu generierten!



-- 3) Sind keine Namenskonflikte da, rufen wir uns rekursiv weiter auf!