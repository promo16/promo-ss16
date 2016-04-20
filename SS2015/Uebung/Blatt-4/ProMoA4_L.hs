module Main where


-- | A4-1

data List a = Leer | Element a (List a)
  deriving Show

class Messbar a where
  messen :: a -> Double

-- a)

instance Messbar (List a) where
  messen Leer          = 0
  messen (Element _ l) = 1 + messen l


-- b) Die Funktion der Eq-Klasse ist '=='

-- (==) :: a -> a -> Bool

instance Eq a => Eq (List a) where
  Leer           == Leer           = True
  (Element x xs) == (Element y ys) = (x == y) && (xs == ys)
  _              == _              = False

-- c)

vergleiche :: (Messbar a, Messbar b) => a -> b -> Ordering
vergleiche x y = messen x `compare` messen y

-- compare :: a -> a -> Ordering

l_1 :: List Int
l_1 = Element 9 Leer

l_2 :: List Char
l_2 = Element 'h' (Element 'a' Leer)

-- Die beiden Listen sind nicht vom selben Typ, man kann sie
-- jedoch trotzdem vergleichen, weil die Klasse Messbar nur die Länge
-- einer Liste misst

e_1 = l_1 `vergleiche` l_2
-- Success: LT

--e_2 = l_1 `compare` l_2
-- ERROR: Couldn't match type 'Char' with 'Int'

e_3 = [1,2] `compare` [9]
-- Success: LT (1 wird mit 9 verglichen)


-- | A4-2

foo :: (Integer, Integer) -> Integer
foo (n, m) | n == 0    = m + 1
           | n >  0    = foo (n - 1, foo (n - 1, m))
           | otherwise = error "1. argument for foo must be not negative."

-- z.z foo (n,m) = 2^n + m

-- Man führt die Rekursion über 'n', weil

-- 1) in jedem Rekursionsaufruf 'n' kleiner wird
-- 2) wir uns keine Sorgen um negative Argumente machen müssen, da diese abgefangen werden

-- Induktionsanfang: n = 0

-- 3) foo (0, m) = m + 1
-- 4) 2^0 + m    = 1 + m

-- ==> 3) == 4)

-- Induktionannahme (IA) : foo (n, m) = 2^n + m

-- Induktionsschritt: n = n + 1

-- 5) neue Funktion zu zeigen: foo (n + 1, m) = 2^(n+1) + m

-- Nun setzen wir 'n + 1' in die Funktion ein und schauen was rauskommt

-- 6) foo (n + 1, m) = foo (n - 1 + 1, foo (n - 1 + 1, m))
--                   = foo (n , foo (n, m))
--    IA)            = foo (n, 2^n + m)
--    IA)            = 2^n + 2^n + m
--                   = 2 * 2^n + m
--                   = 2^(n+1) + m

-- Damit ist 5) bewiesen und wir sind fertig!

-- | A4-3


-- a)

insert :: Int -> [Int] -> [Int]
insert x   []   = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

-- z.z durch Induktion |insert z zs| = 1 + |zs|

-- gegeben:

-- 1) |(x:xs)| = 1 + |xs|
-- 2) Induktion über n aus N, wobei n = |zs|

-- Induktionsanfang: n = 0 (Die Liste zs ist leer)

-- 3) |insert z []| = |[z]| = 1
-- 4) 1 + |zs| = 1 + 0      = 1

-- ==> 3) == 4)

-- Induktionsannahme (IA): |insert z zs| = 1 + |zs|

-- Induktionsschritt: n = n + 1 (Sei 'v' das zusätzliche Element -> (v:zs))

-- 5) neue Funktion zu zeigen: |insert z (v:zs)| = 1 + |(v:zs)|

-- Jetzt kommt die Fallunterscheidung, da 'insert' aus zwei Fällen besteht:

-- 6) Fall 1: z <= v

--   |insert z (v:zs)| = |(z : v : zs)|
--   1)                = 1 + |(v : zs)|

-- Damit ist 5) für den Fall 1 bewiesen!

-- 7) Fall 2: z > v

--  |insert z (v:zs)| = |z : insert v zs|
--  1)                = 1 + |insert v zs|
--  IA)               = 1 + 1 + |zs|

-- Da das Ergebnis nicht ganz perfekt passt, wenden wir 1)
-- 1 + |(v:zs)| = 1 + 1 + |zs]

-- Damit ist 5) für den Fall 2 bewiesen!

-- b)

sort :: [Int] -> [Int]
sort []     = []
sort (x:xs) = insert x (sort xs)

-- z.z |l| = |sort l| durch Induktion

-- gegeben:

-- 1) Teilaufgabe a) darf benutzt werden
-- 2) |l| = |xs| = n

-- Induktionsanfang: n = 0 (Die Liste 'l' ist leer)

-- 3) |l| = 0
-- 4) |sort []| = |[]| = 0

-- ==> 3) == 4)

-- Induktionsannahme (IA): |sort xs| = |xs|

-- Induktionsschritt: n = n + 1 (Sei x das zusätzliche Element -> (x:xs))

-- 5) neue Funktion zu zeigen: |sort (x:xs)| = |(x:xs)|

-- 6) |sort (x:xs)| = |insert x (sort xs)|
--     a)           = 1 + |sort xs|
--    IA)           = 1 + |xs|

-- Da das Ergebnis nicht ganz perfekt passt, wenden wir 1)
-- |(x:xs)| = 1 + |xs|

-- Damit ist 5) bewiesen!


-- A4-4
-- Der Fehler liegt im Induktionsschritt von n=1 auf n=2:

-- Beispiel:
-- Man nimmt aus der Menge '1+1' ein Telefon raus, man nennt es 's'

-- 1) Dann wendet man die Induktionsvoraussetzung darauf an - das heißt
--    dass 'n'-Smartphonens (genau 1) nun alle das gleiche Betriebssystem haben.

-- 2) Nun tun wir dieses eine Smartphone aus der Menge raus (die Menge ist nun leer)
--    und wir tun das 's'-Smartphone in die Menge rein.

-- 3) Damit liegt wieder nur ein Element in der Menge drinnen und nun
--    wenden wir die Voraussetzung auf die Menge an - alle Smartphones haben
--    in der Menge das selbe Betriebssystem

-- 4) Der Fehler passiert dann, wenn wir behaupten wollen, dass die Menge
--    2) und 3) das selbe Betriebssystem haben. Wir haben keine Vergleichselemente
--    in den beiden Mengen gehabt, das heißt für jedes Smartphone an sich, hat
--    jedes das selbe Betriebssystem (weil ja nur eins in der Menge ist).


-- Sei Smartphone 1 (S1) jenes, welche zuerst in 'n' drinlag.
--     Smartphone 2 (S2) jenes, welches als (+1) im Beispiel erklärt wurde
-- ==> Wir sagen das S1 == S1 und S2 == S2, aber wir können keine Meinung darüber
--     treffen ob beide zusammen gleich sind.

-- ==> Der Fehler wäre nicht passiert, wenn wir n=2 setzen würden, da sonst
--     ein Smartphone in der Menge liegen bleiben würde, dass das
--     Betriebssystem "festlägt/festhält"

-- =====> Da aber der Schritt von n=1 -> n=2 fehlgeschlagen ist, sind alle
--        anderen darauf folgenden hinfällig