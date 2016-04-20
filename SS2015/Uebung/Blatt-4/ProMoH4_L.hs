module Main where

import Prelude hiding (concat, zip)

-- | H4-1

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- mit foldl und ausführlicher Lambda-Funktion
concat' :: [[a]] -> [a]
concat' xs = foldl (\ys y -> ys ++ y) [] xs

-- mit foldl und point-free style
concat'' :: [[a]] -> [a]
concat'' = foldl (++) []

-- mit foldr und ausführlicher Lambda-Funktion
concat''' :: [[a]] -> [a]
concat''' xs = foldr (\x xs -> x ++ xs) [] xs

-- mit foldr und point-free style
concat'''' :: [[a]] -> [a]
concat'''' = foldr (++) []

-- mit foldr point-free style
concat''''' :: [[a]] -> [a]
concat''''' = foldl (++) []

-- mit foldr und ausführlicher Lambda-Funktion
concat'''''' :: [[a]] -> [a]
concat'''''' xs = foldr (\x xs -> x ++ xs) [] xs

e_1 = concat       ["Hello ", "World", "!"]
e_2 = concat'      ["Hello ", "World", "!"]
e_3 = concat''     ["Hello ", "World", "!"]
e_4 = concat'''    ["Hello ", "World", "!"]
e_5 = concat''''   ["Hello ", "World", "!"]
e_6 = concat'''''  ["Hello ", "World", "!"]
e_7 = concat'''''' ["Hello ", "World", "!"]

-- | H4-2

zip :: [a] -> [b] -> [(a,b)]
zip []      _     = []
zip _       []    = []
zip (a:as) (b:bs) = (a,b) : zip as bs

-- gegeben:
--          n = |vs|
--          Induktion über n
--          A4-3 -> | (x:xs) | = 1 + |xs|

-- zz. | zip vs ws | = min ( |vs|, |ws| )

--Induktionsanfang: n = 0 => vs = []

-- | zip [] ws | = | [] | = 0

-- min( | [] |, |ws|) = | [] | = 0

-- Induktionsannahme:
--           gegeben: | zip vs ws | = min ( |vs|, |ws| )

-- Sei xs = (v:vs), ys = (w:ws)

--            neu zz. | zip xs ys | = min ( |xs|, |ys| )

-- Induktionsschritt: n -> n + 1

-- | zip xs ys | = | zip (v:vs) (w:ws) |
--               = | (v,w) : zip vs ws |      letzter Fall von zip
--               = 1 + | zip vs ws |          A4-3
--               = 1 + min( |vs|, |ws| )      Induktionsannahme
--               = min (1 + |vs|, 1 + |ws| )  Mathematik
--               = min ( |v:vs|, |w:ws| )     A4-3
--               = min ( |xs|, |ys| )

-- --> bewiesen!

-- | H4-3

data Brotzeit = Leberkas | Weisswurst Int | Breze Brotzeit
  deriving (Show, Eq)

class Messbar a where
  messen :: a -> Double


-- a)

instance Messbar Brotzeit where
  messen Leberkas       = 1
  messen (Weisswurst n) = fromIntegral n / 2
  messen (Breze   rest) = 1 + messen rest

-- b)

-- | Hier ist die Reihenfolge vom Pattern Matching sehr wichtig
--   Wenn 2) oder 3) vor 1) kommt, dann fangen die beiden Fälle
--   auch den Fall mit genau zwei Brezen ab und dann funktioniert
--   der rekursive Aufruf von 1) nicht mehr

--   2) und 3) kann man natürlich vertauschen wie man will

instance Ord Brotzeit where
  compare Leberkas        Leberkas      = EQ
  compare (Weisswurst x) (Weisswurst y) = compare x y

-- 1)
  compare (Breze rest1)  (Breze rest2)  = compare rest1 rest2

-- 2)
  compare (Breze _)      (_)            = GT

-- 3)
  compare (_)            (Breze _)      = LT
  compare Leberkas       (Weisswurst y) = compare 2 y
  compare (Weisswurst x) Leberkas       = compare x 2


b_1 = compare (Breze Leberkas)       (Breze (Weisswurst 1))
b_2 = compare (Breze (Weisswurst 7)) (Breze (Breze Leberkas))
b_3 = compare (Weisswurst 3) Leberkas