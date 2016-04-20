module Main where

-- | H3-1
--          Z          Z          Z
foobar :: Integer -> Integer -> Integer -> String
foobar x y z | even y, z > 0, x < 0 = 'a' : (foobar (1+x) (1+y)  z   )
             | odd  y, z > 0, x < 0 = 'b' : (foobar    x  (y+1) (z-1))
             | otherwise            = show y
-- a)

-- Wieso ist die Abstiegsfunktion f (x,y,z) => max (x+y, 0) nicht geeignet?
-- Gegenbeispiel:

--     x    y  z        (Fall 'odd y')
--     |    |  |               |
-- f ((-1), 1, 3) => max (-1 + 1, 0) = 0

--     x      y      z         (Rekursiver Aufruf von 'odd y')
--     |      |      |               |
-- f ((-1), (1+1), (3-1)) => max(-1 + 1 + 1, 0) = 1

-- ==> 0 > 1 is falsch und damit nicht geeignet

-- b)

-- AUF) Führen die rekursiven Aufrufe aus der definierten Menge raus?
--      Nein - > unsere Definitionsmenge besteht aus: Z x Z x Z
-- DEF) Werden alle möglichen Argumente für unsere Menge abgefangen?
--      Ja -> Wegen dem otherwise-Fall ist die Funktion vollständig definiert
-- ABST) Wähle f (x, y, z) = max ( z - x, 0 )

-- > Erster Fall - even y, z > 0, x < 0:

--               (da z > 0 und x < 0 ist (z - x) > 0)
--                      |
-- f (x, y, z) = max (z - x, 0) = z - x

-- f ((1+x), (1+y), z) = max (z - (x + 1), 0) = z - x - 1

-- ==> z - x > z - x - 1 ist richtig!

-- > Zweiter Fall - odd y, z > 0, x < 0:

-- f (x, y, z) = max (z - x, 0) = z - x

-- f (x, (y+1), (z-1)) = max ((z-1) - x, 0) = z - 1 - x

-- ==> z - x > z - 1 - x is richtig!

-- ====> Damit wird die Funktion in allen Fällen echt kleiner und da der 0 der kleinste
--       Wert ist, muss die Rekursion spätestens da enden, wenn dieser Wert erreicht wird!

-- | H3-2

data List a = Leer | Element a (List a)
  deriving Show

verketten :: List a -> List a -> List a
verketten (Leer)         ys = ys
verketten (Element x xs) ys = Element x (verketten xs ys)


e_1 :: List Int
e_1 = Element 1 (Leer)

e_2 :: List Int
e_2 = Element 2 (Element 3 (Leer))

e_3 :: List Int
e_3 = verketten e_1 e_2


-- | H3-3

data Vergleich = Schlechter
               | Gleich
               | Besser
  deriving Show

data Spiel     = Brettspiel String Int
               | Bausatz Int
  deriving Show


vergleiche :: Spiel -> Spiel -> Vergleich
vergleiche s1 s2
    | s1_players > s2_players = Besser
    | s1_players < s2_players = Schlechter
    | otherwise               = Gleich
  where s1_players = getPlayers s1
        s2_players = getPlayers s2

getPlayers :: Spiel -> Int
getPlayers (Brettspiel _ player) = player
getPlayers (Bausatz       teile) = teile `div` 50


s_1 = vergleiche (Bausatz 212) (Bausatz 243)
  -- Gleich

s_2 = vergleiche (Brettspiel "Nine mens morris" 2) (Brettspiel "Mensch ärgere dich nicht" 4)
  -- Schlechter

s_3 = vergleiche (Bausatz 350) (Brettspiel "Tic-Tac-Toe" 2)
  -- Besser