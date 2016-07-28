-- Aufgabe 10 - 2: Funktoren und Applicative
--

import Data.Char (toUpper)

-- Definition von Funktoren:
--
-- *) in Haskell ist es eine Typklasse mit eine Funktion die bestimmte Regeln befolgen muss und über einem Container agiert
-- *) in der Mathematik kommt dieser Begriff aus der sog. Kategorientheorie die widerrum eine Art der Abstraktion über mathematische Konzepte ist
--    Wenn man Kategorien als Abstraktion von Gruppen / Körpern ansieht, dann ist ein Functor ein Homomorphismus dazwischen (natürlich nicht klausurrelevant)

-- mehr dazu in https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor.html

-- Die Definition der Typklasse in Haskell:

{-

class Functor f where

    fmap :: (a -> b) -> f a -> f b

-}

-- Wenn man Instanzen von dieser Typklasse schreibt sollte man folgende Regeln beachten:

-- 1) fmap id x = x

-- *) Wenn man die Identitätsfunktion auf irgendeinen Container 'f' anwendet, dann sollte er unverändert zurückgegeben werden
-- *) Beispiele:

--    fmap id (Just 4) = (Just 4)     -- der Funktor f ist in diesem Fall der 'Maybe'-Datentyp

--    fmap id [1,3]    = [1,3]        -- der Funktor f ist in diesem Fall der '[]'-Datentyp


-- 2) fmap (f . g) x = fmap f . fmap g $ x

-- *) Mehrmaliges anwenden von 'fmap' sollte keinen Unterschied für die Funktionskomposition machen
-- *) Beispiel:

--    fmap ((+1) . id) (Just 4) = fmap (+1) . fmap id $ (Just 4)
--    \                    __/    \                           /
--     |                  |        \                         /
--     Just (((+1) . id) 4)          fmap (+1) (Just (id 4))
--      \               /             \                   /
--       \             /                fmap (+1) (Just 4)
--        Just ((+1) 4)                  \              /
--         \         /                     Just ((+1) 4)
--          \       /                      \          /
--            Just 5                        \        /
--                                            Just 5


-- a) Implementieren sie einen Listen Datentyp und eine valide Funktorinstanz

data List a = End
            | Element a (List a)

instance Functor List where

--  fmap :: (a -> b) -> List a -> List b
    fmap _  End           = End
    fmap f (Element x xs) = Element (f x) (fmap f xs)

-- b) Schreiben sie eine Show und Eq Instanz für diese Liste

instance Show a => Show (List a) where

--  show :: List a -> String
    show  End           = "[]"
    show (Element x xs) = show x ++ ':' : show xs

instance Eq a => Eq (List a) where

--  (==) :: List a -> List a -> Bool
    (==) (Element x xs) (Element y ys) = x == y && xs == ys
    (==)  End            End           = True
    (==)  _              _             = False


-- c) Implementieren sie eine Function 'scale :: (Functor f, Num b) => b -> f b -> f b'

scale :: (Functor f, Num b) => b -> f b -> f b
scale  factor container = fmap (*factor) container

-- oder das Lambda ausgeschrieben
scale' factor container = fmap (\x -> x * factor) container

-- λ> scale 4 $ toList [1,2,3,4,5]
-- 4:8:12:16:20:[]
-- it :: (Num b) => List b

-- d) Implementieren sie die Applicative Instanz für 'List'

-- Definition von Applicative

-- *) in Haskell ist es eine Typklasse mit zwei Funktionen die auch bestimmte Regeln befolgen müssen
--    da es etwas viel zum Schreiben ist - hier sind diese gehighlighted http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html

-- Definition dieser Typklasse:

{-

class Functor f => Applicative f where

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b
-}


-- Die erste Funktion 'pure' nimmt einen Wert und verpackt ihn in den Container
-- 
-- *) pure 4 :: Maybe Int => (Just 4)
-- *) pure 4 :: [Int]     => [4]

-- Die zweite Funktion '<*>' (manchmal "applied over" genannt) nimmt als 
--  *) erstes Argument:  Ein Container wo Funktion(en) drin sind (in Form von 'a -> b')
--  *) zweites Arguemt:  Ein Container wo Werte drin sind        (in Form von 'a')
-- und gibt uns den einen Container von 'b'-Werten zurück

-- Das macht es etwas einfacher zu verstehen, was passiert - wir entpacken die Funktionen, dann entpacken wir die Werte
-- und wenden die Funktionen auf die Werte nach der Reihenfolge an. Am Schluss verpacken wir es wieder in den Container

-- Wenn ihr das Blatt 11 bereits angeschaut habt, können wir die Implementierung von (<*>) folgendermaßen (für z.B. Maybe) hinschreiben:

--     (<*>) verpackteFunktion verpackterWert = do
--             (funktion :: a -> b) <- verpackteFunktion :: Maybe (a -> b)
--             (wert :: a)          <- verpackterWert    :: Maybe a 
--             let result = funktion wert :: b
--             (pure result) :: Maybe b

-- *) Just (\x -> x + 1)             <*> Just 4   => Just 5
-- *) [(\x -> x + 1), (\y -> y + 2)] <*> [2, 5]   => [3, 6, 4, 8]

instance Applicative List where

--  pure :: a -> List a
    pure x = Element x End

--  (<*>) :: List (a -> b) -> List a -> List b
    (<*>)  End             _             = End
    (<*>)  _               End           = End
    (<*>) (Element f fs)  (Element x xs) = Element (f x) (fs <*> xs)

-- Wenn man sich die zipWith Funktion vom Blatt 8 in Erinnerung ruft, sieht diese etwas ähnlich aus
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f  []    _      = []
-- zipWith f  _     []     = []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- Für Aufmerksame - diese Implementierung folgt einer der Regeln für Applicative nicht - welcher?

lzipWith :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
lzipWith f xs ys = (fmap f xs) <*> ys   

-- Was passiert hier?

-- *) f :: (a -> b -> c)
-- *) xs :: Applicative f => f a          (ein Container der 'a's enthält)
-- *) ys :: Applicative f => f b          (ein Container der 'b's enthält)

-- *) fmap f xs :: Applicative f => f (b -> c)
--       **) Hier wird die Funktion 'f' auf jedes Element aus 'xs' angewendet.
--       **) 'f' nimmt aber zwei Argumente 'a' und 'b'
--       **) xs hat aber lediglich nur 'a' - Werte
--       **) Durch partielle Applikation machen wir aus 'a -> b -> c' eine Funktion 'b -> c' indem wir ihr ein 'a' aus 'xs' geben
--       **) Da wir 'fmap' benutzen, wird 'f' auf jedes Element im Container angewendet und wir kriegen einen Container mit Funktionen
--           vom Typ 'Applicative f => f (b -> c)' zurück

-- *) (fmap f xs) <*> ys
--       **) 'fmap f xs' ist ein Container von Typ 'Applicative f => f (b -> c)' und 'ys' ein Container mit dem Typ 'Applicative f => f b'
--       **) Wir wenden die Funktionen aus 'fmap f xs' vom Typ 'b -> c' auf jedes Element aus 'ys' vom Typ 'b' an
--       **) Dadurch machen wir aus der Funktion 'b -> c' und dem Typ 'b' den Typ 'c' - wir kriegen also Werte vom Typ 'c' zurück
--       **) Am Ende wird alles wieder in den Container gepackt und als 'Applicative f => f c' zurückgegeben

-- Beispiel:

-- lzipWith (+) (Element 1 (Element 2 End)) <*> (Element 4 (Element 5 End))

-- => fmap (+) (Element 1 (Element 2 End)) <*> (Element 4 (Element 5 End))
      -----------------------------------

--    => fmap (+) (Element      1            (Element      2            End))     (in der Functor Instanz nachschauen)
--    =>           Element ((+) 1) (fmap (+) (Element      2            End))
--    =>           Element ((+) 1)           (Element ((+) 2)  fmap (+) End))
--    =>           Element ((+) 1)           (Element ((+) 2)           End))
--- => Element ((+) 1) (Element ((+) 2) End)) <*> (Element 4 (Element 5 End))      (in der Applicative Instanz nachschauen)
--             |     | ___________________________________/
--             |    / /
-- => Element ((+) 1 4) ((Element ((+) 2) End) <*> (Element 5 End))
--                                /    / __________________/
--                               |    | |      
-- => Element ((+) 1 4) (Element ((+) 2 5) (End <*> End))
--
-- => Element ((+) 1 4) (Element ((+) 2 5) End)
--            --------            -------
-- => Element 5 (Element 7 End)

-- Beispiel:
    
--    Just (\x -> x + 5) <*> Just 4
-- => Just ((\x -> x + 5) 4)
-- => Just (4 + 5)
-- => Just 9

-- Hilfsfunktion zum Testen

toList :: [a] -> List a
toList []     = End
toList (x:xs) = Element x (toList xs)



-- Übungen - was ist das Ergebnis, bzw. was ist der Typ?:

u0 = fmap (+1) (Just 4)

u1 = fmap (+1) Nothing

u2 = (1-) <$> Nothing

u3 = fmap (*2) [1,2,3]

u4 = fmap (*) [1,2,3]

u5 = fmap head getLine

u6 = fmap (toUpper . head) getLine

u7 = pure 1    :: Int -> Int   -- was ist das Ergebnis vom Ausdruck 'u7 5'

u8 = pure 5    :: [Int]

u9 = pure True :: Maybe Bool


u10 = Just (\x -> x `mod` 2 == 0) <*> Just 4


u11 = Just odd <*> Just 4


u12 = [f,g] <*> [1,2,3]
    where f x = x - 1
          g x = x * 2

u13 = [f,g,id] <*> [1,2,3]
    where f x = x - 1
          g x = x * 2

u14 = fmap (++) (Just []) <*> Just "hello"

u15 = fmap (++) (Just []) <*> Nothing

u16 = fmap (++) (pure []) <*> Just "world"

u17 x = pure (.) <*> Just (\x -> x + 1) <*> Just (\y -> y * 2) <*> x    -- was ist das Ergebnis vom Ausdruck 'u17 (Just 4)'

{-
-- zur Erinnerung, ein paar Instanzen die wir hier benutzen (sollte man wahrscheinlich nicht auswendig wissen, aber bereits mal gesehen haben)
--
-- zu jeder Instanz ist die Herleitung der Typen hingeschrieben - diese sollte man lediglich nachvollziehen können
--

instance Functor Maybe where

--  fmap :: (a -> b) -> f a      -> f b        (Ausgangstyp)
--  fmap :: (a -> b) -> Maybe a  -> Maybe b    (Einsetzen)

    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where


--  pure :: a -> f a
--  pure :: a -> Maybe a

    pure x = Just x

--  (<*>) :: f (a -> b)     -> f a     -> f b        (Ausgangstyp)
--  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b    (Einsetzen)
    
    (<*>) (Just f) (Just x) = Just (f x)
    (<*>) _        _        = Nothing


instance Functor ((->) r) where

--  fmap :: (a -> b) -> f a      -> f b        (Ausgangstyp)
--  fmap :: (a -> b) -> (->) r a -> (->) r b   (Einsetzen)
--  fmap :: (a -> b) -> (r -> a) -> (r -> b)   (Auswerten)
--  fmap :: (a -> b) -> (r -> a) -> r -> b     (letzte Klammer fällt weg, weil das bereits implizite Klammerung ist)
--         /______/     /_____/    /
--        /  __________/          /
--       |  / ___________________/
--       | | |
    fmap f g x = (f . g) x

instance Applicative ((->) r) where

--  pure :: a -> f a       (Ausgangstyp)
--  pure :: a -> (->) r a  (Einsetzen)
--  pure :: a -> (r -> a)  (Auswerten)
--  pure :: a -> r -> a    (Implizite Klammerung am Rückgabewert entfernen)
--        _/    /
--       |  ___/
--       | |
    pure x _ = x     -- alternativ: pure x = const x


--  (<*>) :: f (a -> b) -> f a -> f b                   (Ausgangstyp)
--  (<*>) :: (->) r (a -> b) -> (->) r a -> (->) r b    (Einsetzen)
--  (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)    (Auswerten)
--  (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)    (Implizite Klammerung in der inneren Funktion entfernen)
--  (<*>) :: (r -> a -> b)   -> (r -> a) -> r -> b      (Implizite Klammerung in beim Rückgabewert entfernen)
--          /___________/       /_____/    /
--         / __________________/          /
--        | |  __________________________/
--        | | |
    (<*>) f g x = f x (g x)

instance Functor [] where

--  fmap :: (a -> b) -> f a  -> f b     (Ausgangstyp)
--  fmap :: (a -> b) -> [] a -> [] b    (Einsetzen)
--  fmap :: (a -> b) -> [a]  -> [b]      (Auswerten)

    fmap f []     = []
    fmap f (x:xs) = f x : fmap f xs

instance Applicative [] where

--  pure :: a -> [a]
    pure x = [x]

--  (<*>) :: f (a -> b) -> f a -> f b      (Ausgangstyp)
--  (<*>) :: [] (a -> b) -> [] a -> [] b   (Einsetzen)
--  (<*>) :: [(a -> b)] -> [a] -> [b]      (Auswerten)

    (<*>) fs xs = [f x | f <- fs, x <- xs]

instance Functor IO where

--  fmap :: (a -> b) -> f a  -> f b     (Ausgangstyp)
--  fmap :: (a -> b) -> IO a -> IO b    (Einsetzen)

    fmap f action = do
        x <- action
        return $ f x

instance Applicative IO where

--  pure :: a -> IO a
    pure x = return x

--  (<*>) :: IO (a -> b) -> IO a -> IO b
    (<*>) funaction action = do
        f <- funaction
        x <- action
        return $ f x

-}



-- Lösungen:
{-

u0 :: Num a => Maybe a
u0 = fmap (+1) (Just 4)    => 5

u1 :: Num a => Maybe a
u1 = fmap (+1) Nothing     => Nothing

u2 :: Num a => Maybe a
u2 = (1-) <$> Nothing      => Nothing

u3 :: Num a => [a]
u3 = fmap (*2) [1,2,3]     => [2,4,6]

u4 :: Num a => [a -> a]
u4 = fmap (*) [1,2,3]      => Es existiert keine Show-Instanz für Funktionen

u5 :: IO Char
u5 = fmap head getLine              => Eingabe: "hello", Ausgabe: 'h'

u6 :: IO Char
u6 = fmap (toUpper . head) getLine  => Eingabe: "hello", Ausgabe: 'H'

u7 :: Int -> Int
u7 = pure 1                  'u7 5' => 1  (Siehe Z.304)

u8 :: [Int]
u8 = pure 5                         => [5]

u9 :: Maybe Bool
u9 = pure True                      => Just True

u10 :: Maybe Bool
u10 = Just (\x -> x `mod` 2 == 0) <*> Just 4     => Just True

u11 :: Maybe Bool
u11 = Just odd <*> Just 4                        => Just False

u12 :: Num a => [a]
u12 = [f,g] <*> [1,2,3]                          => [f 1, f 2, f 3, g 1, g 2, g 3] => [0,1,2,2,4,6]
    where f x = x - 1
          g x = x * 2

u13 :: Num a => [a]
u13 = [f,g,id] <*> [1,2,3]                       => [f 1, f 2, f 3, g 1, g 2, g 3, id 1, id 2, id 3] => [0,1,2,2,4,6,1,2,3]
    where f x = x - 1
          g x = x * 2

u14 :: Maybe String
u14 = fmap (++) (Just []) <*> Just "hello"       => Just ((++) [] "hello") => Just "hello"

u15 :: Maybe String
u15 = fmap (++) (Just []) <*> Nothing            => Just ((++) []) <*> Nothing => Nothing

u16 :: Maybe String
u16 = fmap (++) (pure []) <*> Just "world"       => fmap (++) (Just []) <*> Just "world" => Just "world"

u17 :: Num a => Maybe a -> Maybe a
u17 x = pure (.) <*> Just (\x -> x + 1) <*> Just (\y -> y * 2) <*> x

    'u17 (Just 4)'
  
    Sei f = (\x -> x + 1)
        g = (\y -> y * 2)

 => pure (.) <*> Just f       <*> Just g         <*> Just 4

 => Just (.) <*> Just f       <*> Just g         <*> Just 4

 =>              Just ((.) f) <*> Just g         <*> Just 4

 =>                               Just ((.) f g) <*> Just 4

 =>                               Just (f . g)   <*> Just 4

 =>                                                  Just ((f . g) 4)

 =>                                                  Just (f 8)

 =>                                                  Just 9

-}
