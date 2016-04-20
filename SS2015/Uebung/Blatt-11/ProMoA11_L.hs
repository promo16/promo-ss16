module Main where

import Prelude hiding (iterate)
import Control.Monad
import Control.Monad.Par

-- | A11-1 Auswertungsstrategie

-- Immer den Redex (ein zu reduzierender Ausdruch) unterstreichen!

-- Tipp für für die Hausaufgabe - implizite Linksklammerung für Funktionsanwendungen beachten!
-- Man darf erst unter einem Lambda reduzieren, wenn alles andere bereits reduziert worden ist!


-- a)

-- (\(x, y) -> y) (1 + (2 + 3), 4 + 5)


-- Call-by-Name

--    (\(x, y) -> y) (1 + (2 + 3), 4 + 5)
-- => 4 + 5
-- => 9

-- Call-by-Value

--    (\(x, y) -> y) (1 + (2 + 3), 4 + 5)
-- => (\(x, y) -> y) (1 + 5, 4 + 5)
-- => (\(x, y) -> y) (6, 4 + 5)
-- => (\(x, y) -> y) (6, 9)
-- => 9

-- b)

-- (\x -> x + x) ((\y -> y * y) (1 + 1))

-- Call-by-Name

--  (\x -> x + x) ((\y -> y * y) (1 + 1))
-- => ((\y -> y * y) (1 + 1)) + ((\y -> y * y) (1 + 1))
-- => ((1 + 1) * (1 + 1))     + ((\y -> y * y) (1 + 1))
-- => (2 * (1 + 1))           + ((\y -> y * y) (1 + 1))
-- => (2 * 2)                 + ((\y -> y * y) (1 + 1))
-- => 4                       + ((\y -> y * y) (1 + 1))
-- => 4                       + ((1 + 1) * (1 + 1))
-- => 4                       + (2 * (1 + 1))
-- => 4                       + (2 * 2)
-- => 4                       + 4
-- => 8

-- Call-by-Value

--  (\x -> x + x) ((\y -> y * y) (1 + 1))
-- => (\x -> x + x) ((\y -> y * y) 2)
-- => (\x -> x + x) (2 * 2)
-- => (\x -> x + x) 4
-- => 4 + 4
-- => 8


-- A11-2 Hasse Diagramm (alte Aufgabe)

-- Die Aufgabe war hier ein Hesse-Diagramm für den Datentyp:

-- data Entweder = EinB  Bool
--               | Azahl Int

-- Das Beispiel für 'Maybe Bool':

--                  Just True   Just False
--                         \     /
--    Nothing               \   /
--        \                  \ /
--         \               Just _|_
--          \                 /
--           \   ____________/
--            \ /
--            _|_

-- _|_ - Dieses Zeichen nennen wir 'bottom'

-- Wir schauen wie wir ein sog. 'thunk' auswerten - und was für eine Form er
-- annehmen kann:

-- x1 = undefined
-- x2 = Nothing
-- x3 = Just undefined
-- x4 = Just True
-- x5 = Just False


-- y1 = undefined
-- y2 = EinB undefined
-- y3 = Azahl undefined
-- y4 = EinB True
-- y5 = EinB False
-- y6 = Azahl 0
-- y7 = Azahl 1
-- y8 = Azahl (-1)
-- y9 = Azahl 2
--y10 = Azahl ...

--      EinB True   EinB False      Azahl 0  Azahl 1 Azahl -1  Azahl 2    ...
--            \     /                    \      /       /         /       /
--             \   /                      \    /       /         /       /
--              \ /                        \  /_______/_________/_______/
--               |                          \//
--            EinB _|_                   Azahl _|_
--                 \           _____________/
--                  \         /
--                   \       /
--                    \     /
--                     \   /
--                      _|_


-- | A11-2 Poset


-- Was ist ein Poset?

-- Eine partiell geordnete Menge mit einer partiellen binären Ordnungsrelation

-- die Ordnungsrelation muss 3 Eigenschaften erfüllen:

-- Sei M eine partiell geordnete Menge
-- Sei R eine binäre Relation
-- Sei x, y, z € M

-- Transitivität:

--   x R y && y R z <=> x R z

-- Bsp:

--  1 < 2 && 2 < 3 <=> 1 < 3

-- Antisymmetrie:

-- x R y && y R x <=> x = y

-- Bsp:

-- 1 <= 1 && 1 >= 1 <=> 1 = 1

-- a | b && b | a   <=> b = a (ganzzähliger Teiler)

-- Reflexiv:

-- x R x

-- Bsp:

--  1 <= 1
--  2 =  2

-- a) (N, /=)

-- => /= ist nicht reflexiv!

-- 3 /= 3 stimmt nicht!


-- b) ({ Schere, Stein, Papier }, ~~ ), wobei ~~ := { (Stein, Stein),  (Schere, Schere), (Papier, Papier),
--                                                    (Stein, Schere), (Schere, Papier), (Papier, Stein)}

-- Man kann hieraus ableiten, dass ~~ eine Relation ist, die bedeutet ' verliert nicht gegen '!

-- Hier trifft die Transitivität nicht zu!

-- Stein ~~ Schere && Schere ~~ Papier <=> Stein ~~ Papier    <-- stimmt aber nicht!


-- c) ({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, |), wobei x | y := x ist ganzzahliger Teiler von y

-- Hier treffen alle Eigenschaften zu!


--              8
--              |
--              4  6  9
--              |_/ \_|
--              2     3
--               \   /           5                 7
--                \ /____________/________________/
--                 1


-- Tipp für die Hausaufgabe - 'P' also Potenzmenge ist in Haskell durch 'subsequences' aus Data.List definiert


-- | A11-3 Umgang mit Monaden

-- a)

f1 = mapM (\x -> if even x then Just x else Nothing) [1..3]

--       Nothing >>= \_ ->  Just 2 >>= \_ -> Nothing
--    => Nothing

f2 = mapM print [1..4]

-- print x = putStrLn $ show x :: Show a => a -> IO ()

-- =>
-- 1
-- 2
-- 3
-- 4
-- [(), (), (), ()]

f3 = forM [1..3] (\x -> print x >> return (x + 1))

-- =>
-- 1
-- 2
-- 3
-- [2, 3, 4]

f4 :: [Int]
f4 = runPar $ do

-- [IVar Int]              Par [IVar Int]
-- |     ______________________|___________________
-- |    /                                          \
  xs <- forM ([1..3] :: [Int]) (\x -> spawnP (x * x))


-- Par [Int]
--  ____|___
-- /        \
  mapM get xs


-- => [1, 4, 9]

-- | A11-4 Lazy Evaluation

ps = iterate (+1) 0
qs = iterate (*2) 1
rs = iterate (^2) 2

iterate :: (Int -> Int) -> Int -> [Int]
iterate f x = x : iterate f (f x)


foo :: [Int] -> [Int] -> [Int] -> Int
foo (_:x:xs) ys@(y:_) zs
  | x > y     = x
  | otherwise = foo ys zs xs


-- zu was evaluiert 'foo ps qs rs'?

-- ps => [0, 1, 2, 3, 4...]

-- qs => [1, 2, 4, 8, 16...]

-- rs => [2, 4, 16, 256, 65536...]


--        ps            qs           rs
-- foo [_,1,2,3...] [1,2,4,8...] [2,4,16...]
--       /   _______/
--      |   |
--   => 1 > 1 = False
--            qs          rs        ps
--   => foo [1,2,4...] [2,4,16...] [2,3...]

--             qs         rs         ps
--   => foo [_,2,4...] [2,4,16...] [2,3...]
--            /      /
--           /    __/
--          /    /
--   =>     2 > 2 = False

--             rs          ps         qs
--  => foo [_,4,16...] [2,3,4...] [4,8,16...]
--         __/         /
--        /   ________/
--       /   /
--  =>  4 > 2 = True => 4