module Main where


import Prelude hiding (IO)

-- | A8-1

-- Sei unify = u

-- a) { Int -> (A -> B) = A -> (B -> Int) }

--    u { Int -> (A -> B) = A -> (B -> Int) }

-- => u { Int = A, A -> B = B -> Int }

-- => u { Int = A, A = B, B = Int }

--    Ersetze A mit Int

-- => [Int/A] u { Int = Int , Int = B, B = Int }

--    A = A oder Int = Int fällt weg

-- => [Int/A] u { Int = B, B = Int }

--    Ersetze B mit Int

-- => [Int/A, Int/B] u { Int = Int , Int = Int }

-- => [Int/A, Int/B] u {}

-- => [Int/A, Int/B] ist damit der allgemeinste Unifikator


-- b) { A -> (A -> Int) = (Int -> B) -> B }

--    u { A -> (A -> Int) = (Int -> B) -> B }

-- => u { A = Int -> B, A -> Int = B }

--    Ersetze A mit (Int -> B)

-- => [(Int -> B)/A] u {Int -> B = Int -> B, (Int -> B) -> Int = B}

-- => [(Int -> B)/A] u {(Int -> B) -> Int = B}

--    Ersetze B mit ((Int -> B) -> Int)

-- => [(Int -> B)/A), ((Int -> B) -> Int)/B] u {((Int -> (Int -> B) -> Int)) -> Int = (Int -> B) -> Int}

--    man sieht dass wir hier in einen zirkulären Typ gefangen sind, da auf beiden
--    Seiten ein 'B' vorkommt und wir mit jeder Unifikation uns nur vergrößern

-- => Keine Lösung - zirkulärer Typ


-- c) { A -> (A -> Int) = (Int -> B) -> Y }

--    u { A -> (A -> Int) = (Int -> B) -> Y }

-- => u { A  = Int -> B,  A -> Int = Y }

--   Ersetze A mit (Int -> B)

-- => [(Int -> B)/A] u { Int -> B  = Int -> B,  (Int -> B) -> Int = Y }

-- => [(Int -> B)/A] u { (Int -> B) -> Int = Y }

--   Ersetze Y mit ((Int -> B) -> Int)

-- => [(Int -> B)/A, ((Int -> B) -> Int)/Y] u { (Int -> B) -> Int = (Int -> B) -> Int }

-- => [(Int -> B)/A, ((Int -> B) -> Int)/Y] u {}

-- => [(Int -> B)/A, ((Int -> B) -> Int)/Y] ist damit der allgemeinste Unifikator


-- | A8-2

-- a) Baumstruktur

-- {} |- (\x -> (\y -> (\z -> x z y))) :: (A -> B -> D) -> B -> A -> D

-- Zuerst immer die implizite Klammerung beachten!

--      Funktionsanwendung (impl. links)   Typ (impl. rechts)
--                           |                     |
-- {} |- (\x -> (\y -> (\z -> (x z) y))) :: (A -> (B -> D)) -> B -> A -> D

-- Wird von unten nach oben aufgebaut:

--                            x :: A -> B -> D € K         z :: A € K
--                            _____________________ Var)    ___________ Var)

--    y :: B € K              K |- x :: A -> B -> D         K |- z :: A
--    ____________ Var)        _____________________________________ App)

--    K |- y :: B                       K |- x z :: B -> D
--    ______________________________________________________________ App)

--         { x :: A -> B -> D, y :: B, z :: A } |- (x z) y :: D       -- Sei der ganze Kontext ( alles zwischen {})
--                                                                      -- nun K
--        _________________________________________________________ Abs)

--        { x :: A -> B -> D, y :: B } |- (\z -> (x z) y) :: A -> D

--     ______________________________________________________________ Abs)

--     { x :: A -> B -> D } |- (\y -> (\z -> (x z) y)) :: B -> A -> D
-- _________________________________________________________________________  Abs)

-- {} |- (\x -> (\y -> (\z -> (x z) y))) :: (A -> (B -> D)) -> B -> A -> D


-- Die ist die Funktion 'flip'


-- b)

-- { x :: Bool, y :: Double -> Int } |- (\z -> z x 4 (y 7)) :: (Bool -> Int -> Int -> B) -> B

-- -- Klammerung nicht vergessen!
-- --                                          imp. links                    imp. rechts
-- --                                              _|_                         ____|____
-- --                                             |   |                       |         |
-- { x :: Bool, y :: Double -> Int } |- (\z -> ((z x) 4) (y 7)) :: (Bool -> (Int -> (Int -> B))) -> B

-- Nr.
-- 1) { x :: Bool, y :: Double -> Int } |- (\z -> ((z x) 4) (y 7)) :: (Bool -> (Int -> (Int -> B))) -> B

-- (Abs 2) bedeutet wir benutzen die Abstraktion und schreiben sie in Zeile 2

-- 2) { x :: Bool, y :: Double -> Int, z :: Bool -> (Int -> (Int -> B)) } |- ((z x) 4) (y 7) ::  B

-- Sei der Kontext (alles was zwischen den {} steht) nun K

--    K |- ((z x) 4) (y 7) ::  B            (App 3,4)

-- 3) K |- (z x) 4 :: Int -> B              (App 5,6)

-- 4) K |- y 7 :: Int                       (App 7,8)

-- 5) K |- z x :: Int -> (Int -> B)         (App 9, 10)

-- 6) K |- 4   :: Int                       (Const)

-- 7) K |- y   :: Double -> Int             (Var)

-- 8) K |- 7   :: Double                    (Const)

-- 9) K |- z :: Bool -> (Int -> (Int -> B)) (Var)

-- 10) K |- x :: Bool                       (Var)



-- c)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Umformung zu unserer Lambda-Darstellung

twice' = \f -> (\x -> f (f x))


-- {} |- \f -> (\x -> f (f x)) :: (A -> A)-> A -> A

-- In der Musterlösung ist die Baumstruktur gegeben - ich bevorzuge die
-- lineare Notation

-- 1) {} |- \f -> (\x -> f (f x)) :: (A -> A)-> A -> A     (Abs 2)

-- 2) { f :: A -> A } |- \x -> f (f x) :: A -> A           (Abs 3)

-- 3) { f :: A -> A, x :: A } |- f (f x) :: A

-- Sei K der Kontext

--    K |- f (f x) :: A    (App 4, 5)

-- 4) K |- f :: A -> A     (Var)

-- 5) K |- f x :: A        (App 6, 7)

-- 6) K |- f :: A -> A     (Var)

-- 7) K |- x :: A          (Var)



-- | A8-3


type Welt = Integer

type IO a = (Welt -> (a, Welt))


nacheinander :: IO a -> IO b -> IO b
nacheinander f = komposition f . const

-- Für später: (>>)

-- analog!

-- nacheinander führt zuerst die erste Funktion aus - dann die zweite!
nacheinander' :: (Welt -> (a, Welt)) -> (Welt -> (b, Welt)) -> Welt -> (b, Welt)
nacheinander' f g w1 = (y, w3)
  where (_, w2) = f w1 -- wir schmeißen das Ergebnis hier weg!
        (y, w3) = g w2

--                        f)                         g)         die vorherige Welt
--                ________|________       ___________|___________       |
--               |                 |     |                       |      |
--komposition :: (Welt -> (a, Welt)) -> (a -> (Welt -> (b, Welt))) -> Welt -> (b, Welt)


-- Für später: (>>=)

komposition :: IO a -> (a -> IO b) -> IO b
komposition f g w1 = (y, w3)
  where (x, w2) = f w1
        (y, w3) = g x w2 -- Berechnung der neuen Welt nachdem
                         -- f darauf Einfluss genommen hat
                         -- Hier wird das Ergebnis gebraucht!


-- Aufpassen - point-free style!

-- demoSequenz :: Welt -> ((), Welt)
demoSequenz :: IO ()
demoSequenz = nacheinander' tick $
              nacheinander' tick $
              nacheinander' tick $
              komposition lese addiere

-- analog

demoSequenz' :: Welt -> ((), Welt)
demoSequenz' w = nacheinander' tick (
                   nacheinander' tick (
                     nacheinander tick (
                       komposition lese addiere
                     )
                   )
                 ) w

makeWorld :: Welt
makeWorld = 0

-- lese :: Welt -> (Int, Welt)
lese :: IO Int
lese w = (fromIntegral w, w)


-- tick :: Welt -> ((), Welt)
tick :: IO ()
tick w = ((), succ w)


-- addiere :: Int -> Welt -> ((), Welt)
addiere :: Int -> IO ()
addiere x w = ((), w + (fromIntegral x))


-- Lösung!

demo :: Welt
demo = snd $ demoSequenz makeWorld

-- tick2 :: Welt -> (Int, Welt)
tick2 :: IO Int
tick2 w = (fromIntegral w, w * 3)