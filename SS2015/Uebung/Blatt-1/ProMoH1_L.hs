module Main where

import Prelude hiding (negate, const)

-- H1-1

-- Gesucht: Typ

-- a)

-- [Char] == String

h1_a, h1_a', h1_a'' :: String

h1_a   = ['a','b','c']

h1_a'  = 'a':'b':'c':[]

h1_a'' = "abc"

-- b)

h1_b :: (Char, Char, Char)
h1_b = ('a', 'b', 'c')

-- c)

h1_c :: [(Bool, [Double])]
h1_c = [(False, [1]), (True, [(2.0)])]

-- d)

h1_d :: ([Bool], (Char, Char, Char))
h1_d = ([True, True], ('z', 'o','o'))

-- e)

h1_e, h1_e' :: String -> (String, Bool, [String])

h1_e = (\x -> ('a':x, False, ([x])))

h1_e' x = ('a':x, False, [x])

-- f)

h1_f, h1_f' :: [(Integer, Integer)]

h1_f = [(\x y -> (x*2, y-1)) m n | m <- [1..5], even m, n <- [6..10]]

h1_f' = [(m*2, n-1) | m <- [1..5], even m, n <- [6..10]]


-- H1-2

-- a) Funktionsanwendungen ( <==> alles was hinter dem '=' steht) ist implizit links geklammert

const :: a -> b -> a
const x y = x

negate :: (Num a) => a -> a
negate x = -x

h2_a = const const (negate 1) (negate 2) 3

h2_a' = (((const const) (negate 1)) (negate 2)) 3

-- b) Auswertung

h2_b = const const (negate 1) (negate 2) 3

-- eine Möglichkeit der Auswertung

h2_b_1'    = (const (const) (negate 1)) (negate 2) 3
-- const (const) (negate 1) -> const
--         ^x       ^y           ^x

h2_b_1''   = const (negate 2) 3
-- const (negate 2)  3 -> negate 2
--          ^x      ^y       ^x

h2_b_1'''  = negate 2
-- negate 2 -> -2

h2_b_1'''' = -2

-- eine andere Möglichkeit

h2_b_2     = const const (negate 1) (negate 2) 3
-- negate 1 -> -1

h2_b_2'    = const const (-1) (negate 2) 3
-- negate 2 -> -2

h2_b_2''   = const const (-1) (-2) 3
-- const (const) (-1) -> const
--         ^x     ^y       ^x

h2_b_2'''  = const (-2) 3
-- const (-2)  3 -> (-2)
--        ^x  ^y     ^x

h2_b_2'''' = -2

-- eine andere Möglichkeit (FALSCH nach der Vorlesung)

h2_b_3 = const (const (negate 1) (negate 2)) 3
-- const (negate 1) (negate 2) -> (negate 1)
--          ^x         ^y            ^x

h2_b_3' = const (negate 1) 3
-- const (negate 1)  3 -> (negate 1)
--         ^x       ^y       ^x

h2_b_3'' = negate 1
-- negate 1 -> -1

h2_b_3''' = -1



-- H1-3

-- pi ist in der Standartbibliothek vorhanden
flaecheninhalt :: Double -> Double -> Double
flaecheninhalt a b = pi * a * b

exzentrizitaet :: Double -> Double -> Double
exzentrizitaet a b = sqrt (a^2 - b^2)

umfang :: Double -> Double -> Double
umfang a b = pi * (3/2 * (a + b) - sqrt (a*b))

-- | Dieser Erklärung ist ohne Gewähr
--   Für genauere Definition
--   http://www.zvon.org/other/haskell/Outputprelude/index.html  -> Classes

-- | Es kann ein paar Probleme mit dem Typsystem von Haskell geben,
--   deswegen eine kurze Erklärung zu Typklassen, wann man welche
--   Exponentialfunktion benutzt, da es 3 verschiedene gibt

-- | Was bedeutet "erben"?
--   Die Typklasse A die von einer anderen Typklasse B erbt, dann
--   hat die Funktion die vom Typ A ist auch die Funktionen die die
--   Typklasse B bereitstellt

-- | Num:
--   Ist definiert für Int, Integer, Float und Double
--   Erlaubt Addition (+), Subtraktion (-), Multiplikation (*), Absolutbetrag (abs)
--   Mit (fromInteger) kann man einen Integer in die vorher aufgezählten Typen umwandeln

-- | Real:
--   Erbt von Num
--   Ist definiert für Int, Integer, Float und Double
--   Erlaubt die Darstellung als Bruch mit (toRational)
--   Bsp: toRational 0.5 => 1 % 2

-- | Integral:
--   Erbt von Real und damit auch von Num
--   Ist defniert für Int und Integer (ganze Zahlen)
--   Erlaubt die Division von Ganzzahlen (nach unten gerundet) mit (div), Modulo (mod)
--   Mit (toInteger) kann man aus einem Int einen Integer machen

-- | Fractional:
--   Erbt von Num
--   Ist definiert für Float und Double (Fließkommazahlen)
--   Erlaubt die Division mit (/)

-- | Floating:
--   Erbt von Fractional und damit auch von Num
--   Ist definiert für Float und Double (Fließkommazahlen)
--   Erlaubt die Benutzung von Exponent (**), Wurzel (sqrt), sin, cos, tan...


-- (^)   :: (Integral b, Num a) => a -> b -> a
-- 2^2     = 4
-- 2.0^2   = 4
-- 2^2.0   = error
-- 2.0^2.0 = error
-- > das erste Argument muss vom Typ Int, Integer, Float, Double sein
-- > das zweite Argument muss vom Typ Int oder Integer sein

-- (^^)  :: (Fractional a, Integral b) => a -> b -> a
-- 2^^2    = 4 (hier wird das erste Argument vom Typsystem als Double verstanden)
-- 2.0^^2  = 4
-- 2^^2.0  = error
-- 2.0^2.0 = error
-- > das erste Argument muss vom Typ Float oder Double sein
-- > das zweite Argument muss vom Typ Int oder Integer sein

-- (**)  :: (Floating a) => a -> a -> a
-- 2**2     = 4
-- 2.0**2   = 4
-- 2**2.0   = 4
-- 2.0**2.0 = 4
-- > beide Argumente müssen vom Typ Float oder Double sein

-- Umwandlung von ganzen Zahlen (Int, Integer) zu Reellen (z.B Double)
-- fromIntegral :: (Integral a, Num b) => a -> b
-- fromIntegral 20 -> 20 :: Double

-- Umwandlung von Fließkommazahlen (Float, Double) zu ganzen Zahlen
-- round 21.4 -> 21 :: Integer
-- floor 21.9 -> 21 :: Integer
-- ceiling 21.3 -> 22 :: Integer

