-- Aufgabe 6-3
--
-- Typen einfacher Ausdrücke
--

--a)

a :: Num a => [a]
a = [1, 2, 3]

--b)

b :: String
b = "ab"++ ['c', 'd']

-- Wer vom Blatt kopiert muss die single-quotes um 'c' und 'd' ändern, weil man für Latex ein extra package nutzen muss damit das geht...
 
--c)

c :: Fractional a => [a]
c = [1, 2.0, 3]

--d)

d :: Bool
d = (False && not True) || (not False && True)
--   \_______________/     \________________ /
--         False        ||       True
--
-- => True

--e)

e :: Num a => [(String, a)]
e = [("a", 1), ("b", 2)]

--f)

-- f = [1, 2, [3, 4, [5]]]

-- => Typefehler! Die Elemente in einer Liste müssen vom selben Typ sein!

--g)

g :: Num a => [[[a]]]
g = [[[1], [2]], [], [[3], [4]]]

{-

  > :t [[1], [2]]
  Num a => [[a]]

  > :t [[3], [4]]
  Num a => [[a]]

  > :t []
  [a]

  Die leere Liste kann jeden Typ annehmen! Auch [[a]]

-}
