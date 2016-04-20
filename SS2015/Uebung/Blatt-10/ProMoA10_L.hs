module Main where

import qualified Data.Map as M
import Control.Monad            (when, forM_, mapM_, foldM)
import Control.Applicative
import Data.Function            (on)

-- | A10-1

-- a)

-- Typ von bar :: (Num a, Ord a, Enum a) => a -> a -> a -> a
--                        |____| |____|
--                         /     /
--                        /     /
--                       |     /
a = let bar x y z = if 1 < succ x then z else x + y
    in  bar 0 2 (3 `div` 0)

--     bar 0 2 (3 `div` 0)
--  => if 1 < succ 0 then 3 `div` 0 else 0 + 2
--  => 1 < 1 => False
--  => 0 + 2
--  => 2

-- b)

-- Typ von b :: [(Bool, Integer, Integer)]


--                    1) ?                 Integer      Integer      2) Bool
--             __________|____________     ____|____    ____|____    __|___
--            |                       |   |         |  |         |  |      |
b = [(x,y,z) | x <- [minBound..maxBound], y <- [7, 8], z <- [2, 3], x == even (y + z)]

-- 1) Typ von x ist hier noch nicht entschieden - wir wissen nur das er
--    eine Instanz in der Typklasse Boundend hat

-- 2) Da wir hier gegen einen Bool ein Vergleich (==) anstellen, leitet sich GHC
--    den Typ Bool ist!

-- => minBound :: Bool => False
-- => maxBound :: Bool => True

-- Jetzt können wir mit der Auswertung beginnen:

-- List comprehention verknüpft jedes Element mit jedem andere Element aller Listen
-- unter der Bedingung 'x == even (y + z)'

-- False-Fall:
--   even (7 + 2) = False -> (False, 7, 2)
--   even (7 + 3) = True  -> []
--   even (8 + 2) = True  -> []
--   even (8 + 3) = False  -> (False, 8, 3)

-- True-Fall:
--   even (7 + 2) = False -> []
--   even (7 + 3) = True  -> (True, 7, 3)
--   even (8 + 2) = True  -> (True, 8, 2)
--   even (8 + 3) = False -> []

-- => [(False, 7, 2), (False, 8, 3), (True, 7, 3), (True, 8, 2)]

-- c)

-- Sublime Text nimmt uns durch das Highlightung schon
-- diese Aufgabe ab und zeigt uns was Klammern und was der 'void'-Typ ist

-- wir können den '()'-Typ hier als egal welcher Typ behandeln - z.B Char

--                                          1)
c = (\x y -> reverse $ () : x : () : [()]) (()) ()

-- 1) Hier ist die äußere Klammer eine Klammer und die innere ist ein 'void'-Typ
--    Die ist nur zur Verwirrung da - ohne sie verändert sich das Programm nicht!

-- => (\x y -> reverse $ () : x : () : [()]) (()) ()
-- => (\x y -> reverse $ () : x : () : [()]) () ()

-- Einsetzen für () für x und y
-- => reverse $ () : () : () : [()]

-- => reverse $ () : () : () : () : []

-- => reverse [(), (), (), ()]

-- Da alle Elemente identisch sind, verändert 'reverse' die Liste nicht!

-- => [(), (), (), ()]

-- d)

--   foo :: (a -> a) -> a -> [a]
--             /   ____/
--            /   /                         2)
--           /   |                        __|_
--          |   | |     1)               |    |
d = let foo f = \x -> x : foo f (f x) in take 3 $ foo (*2) 2

-- f wird auf irgendwas angewendet und dann nochmal auf das Ergebnis von sich selber
--  => es muss vom Typ (a -> a) sein

-- f ist abhängig von x => x :: a

-- 1) & 2) es wird irgendein Element an eine Liste angehängt und 'take' funktioniert
-- nur auf Listen => der Ergebnistyp ist [a]

-- Jetzt kommen wir zur Auswertung:

--    foo (*2) 2
-- => 2 : foo (*2) (2*2)
-- => 2 : foo (*2) 4              Zweiter rekursiver Aufruf!
-- => 2 : 4 : foo (*2) (4*2)
-- => 2 : 4 : foo (*2) 8          Dritter rekursiver Aufruf!
-- => 2 : 4 : 8 : foo (*2) (8*2)
-- => u.s.w

-- Nicht vergessen - wir wenden 'take 3' auf das Ergebnis an!

--    take 3 $ 2 : 4 : 8 : foo (*2) (8*2)
-- => [2,4,8]

-- e)

e = let m = M.fromList [('a', 4), ('b', 7), ('c', 6), ('d', 9)]
    in  mapM_ (flip M.lookup m) ['b'..'c']


-- was macht fromList? ( :: Ord k => [(k, a)] -> Map k a )
-- Es erstellt aus einer Liste die aus (Key, Values) besteht eine Map

-- => m :: Num a => Map Char a

-- was macht flip? ( :: (a -> b -> c) -> b -> a -> c )
-- es vertauscht die Argumente von der Funktion auf die sie angwendet wird

-- flip (/) 2 1 => 0.5
-- flip (/) 1 2 => 2

-- was macht lookup? ( :: Ord k => k -> Map k a -> Maybe a )
-- Es schaut nach ob ein Key in einer Map vorkommt und gibt für ein
-- erfolgreiches Ergebnis ein (Just Value) zurück - für nicht erfolgreiches
-- Suchen gibt es ein Nothing zurück

-- was macht mapM_ ( :: Monad m => (a -> m b) -> [a] -> m () )
-- es führt eine monadische Aktion über eine Liste von 'a'´s aus und
-- verwirft das Ergebnis!

-- => hat in diesem Fall den Typ mapM_ :: Num a => (Char -> Maybe a) -> [Char] -> Maybe ()

-- Auswertung:

-- mapM_ (flip M.lookup m) ['b'..'c']

-- Liste kann man auch ohne .. hinschreiben

-- mapM_ (flip M.lookup m) ['b','c']

-- flip kann man anders hinschreiben

-- mapM_ (\x -> M.lookup x m) ['b','c']

-- => M.lookup 'b' m = Just 7
-- => M.lookup 'c' m = Just 6

-- mapM_ packt um das Ergebnis den Just-Konstruktor!

-- (wenn hier mapM) stehen würde, wären wir jetzt fertig

-- => Just [7, 6]

-- und verwirft das Ergebnis!

-- => Just ()


-- | A10-2

-- Nur 1, 5 und 6 sind relevant zur Vorbereitung für die Klausur!

-- | Helper

fizz = cycle ["","","Fizz"]
buzz = cycle ["","","","","Buzz"]
nums = [1..111] :: [Int]

(??) :: Int -> String -> String
x ?? "" = show x
_ ?? y  = y

infixr 9 ??


-- | 1)

fizz1 :: IO ()
fizz1 = forM_ [1..111] $ \x -> do
  let m3 = x `mod` 3 == 0
      m5 = x `mod` 5 == 0
  when m3         $ putStr "Fizz"
  when m5         $ putStr "Buzz"
  if (m3 || m5) then putStr "\n" else print x

-- | 2)

fizz2 :: IO ()
fizz2 = forM_ [1..111] $ \x -> do
  let m3 = x `mod` 3 == 0
      m5 = x `mod` 5 == 0
  if m3 && m5 then putStrLn "FizzBuzz"
  else if m3  then putStrLn "Fizz"
  else if m5  then putStrLn "Buzz"
              else putStrLn $ show x

-- | 3)

fizz3 :: IO ()
fizz3 = mapM_ putStrLn list
  where list = zipWith3 (\x y z -> z ?? (x ++ y)) fizz buzz nums

-- | 4)

fizz4 :: IO ()
fizz4 = mapM_ (putStrLn . (\(x,y) -> x ?? y))  list
  where list = zip [1..111] $ (zipWith (++) `on` cycle) ["","","Fizz"] ["","","","","Buzz"]

-- | 5)

fizz5 :: IO ()
fizz5 = mapM_ putStrLn list
  where list  = map fun [1..111]
        fun x = case (x `mod` 3, x `mod` 5) of
                  (0,0) -> "FizzBuzz"
                  (0,_) -> "Fizz"
                  (_,0) -> "Buzz"
                  _     -> show x

-- | 6)

fizz6 :: IO ()
fizz6 = mapM_ putStrLn [fun x | x <- [1..111]]
  where fun y
          | y `mod` 15 == 0 = "FizzBuzz"
          | y `mod` 5  == 0 = "Buzz"
          | y `mod` 3  == 0 = "Fizz"
          | otherwise       = show y



-- | A10-3


data Entweder a b = Eines b | Anderes a
  deriving (Show, Eq, Ord)


-- Was ist hier zutun?

e1 = (*) <$> Eines 3 <*> Eines 4

-- => Eines 12

muu :: Int -> Int ->  Entweder String Int
muu x y = if y > 0 then Eines (x `div` y)
                   else Anderes "Div-by-Zero"

e2 = foldM muu 100 [2, 5, 3]

-- => Eines 3

e3 = foldM muu 100 [2, 5, 0, 3]

-- => Anderes "Div-by-Zero"

instance Functor (Entweder a) where
    fmap f (Eines a)   = Eines   $ f a
    fmap f (Anderes a) = Anderes     a

instance Applicative (Entweder a) where
    (Eines   f) <*> a = fmap f a
    (Anderes b) <*> _ = Anderes b

    pure a = Eines a

instance Monad (Entweder a) where
  return = pure
  Eines a   >>= f = f a
  Anderes a >>= _ = Anderes a