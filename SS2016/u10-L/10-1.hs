-- Aufgabe 10 - 1: Monoide
--
--
--
-- Definition von Monoiden:
--
-- *) in Haskell ist es eine Typklasse mit zwei Funktionen und bestimmten Regeln die diese befolgen müssen
-- *) in der Mathematik ist es eine algebraische Struktur mit einer assoziativen binären Relation und der Identität

-- Definition der Typklasse in Haskell:

{-

class Monoid a where

  mempty :: a
  mappend :: a -> a -> a

-}

-- Wenn man Instanzen von diese Typklasse schreibt, sollte man folgende Regeln beachten:

-- 1) mempty `mappend` x = x = x `mappend` mempty

-- *) Das heißt, dass mempty das links-, sowie das rechtsneutrale Element bezüglich mappend ist.
-- *) Beispiele:

--    **) 0  +   5      =  5      =  5      +  0   (in diesem Fall ist mempty = 0  und mappend = (+))
--    **) 1  *   5      =  5      =  5      *  1   (in diesem Fall ist mempty = 1  und mappend = (*))
--    **) [] ++ [1,2,3] = [1,2,3] = [1,2,3] ++ []  (in diesem Fall ist mempty = [] und mappend = (++))

-- 2) (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- *) Das heißt, dass die Funktion mappend assoziativ ist
-- *) Beispiele:

--     **) ( 1  +   2 ) +   3  =  6      = 1   +  ( 2  +   3 )   (in diesem Fall ist mappend = (+))
--     **) ( 1  *   2 ) *   3  =  6      = 1   *  ( 2  *   3 )   (in diesem Fall ist mappend = (*))
--     **) ([1] ++ [2]) ++ [3] = [1,2,3] = [1] ++ ([2] ++ [3])   (in diesem Fall ist mappend = (++))

-- Diese Struktur ist sehr oft nützlich, wenn wir default Werte für bestimmte Typen automatisch ableiten wollen (mempty)
-- und die Assoziativität der Funktion erlaubt uns effizientere Algorithmen zu entwickeln, die in undefinierter Reihenfolge
-- arbeiten können, weil diese eben egal ist

-- Wer zu faul ist die ganze Zeit `mappend` zu schreiben, kann die äquivalente Funktion '<>' (aus Data.Monoid) benutzen:
--
--   *) "hallo" <> " " <> "welt!" => "hallo welt!"

-- a) implementieren sie den Datentyp CharacterChain der Strings darstellt

data CharacterChain = CharacterChain [Char]
    deriving Show

instance Monoid CharacterChain where

--  mempty :: CharacterChain
    mempty = CharacterChain []

--  mappend :: CharacterChain -> CharacterChain -> CharacterChain
    mappend (CharacterChain xs) (CharacterChain ys) = CharacterChain (xs ++ ys)


-- b) implementieren sie den Datentyp ComplexNumber mit passender String-Darstellung

data ComplexNumber = CNum { real :: Double, img :: Double }


instance Monoid ComplexNumber where

--  mempty :: ComplexNumber
    mempty = CNum { real = 0, img = 0 }

--  mappend :: ComplexNumber -> ComplexNumber -> ComplexNumber
    mappend (CNum r1 i1) (CNum r2 i2) = CNum (r1 + r2) (i1 + i2)

instance Show ComplexNumber where

--  show :: ComplexNumber -> String
    show (CNum r i) = show r ++ imgPart ++ "i"
        where

            imgPart
              | i >= 0    = " + " ++ show i
              | otherwise = " - " ++ show (negate i)

-- c) RGB Farbraum kodieren mit additiver Farbmischung

data RGB = RGB (Int, Int, Int)

instance Monoid RGB where

--  mempty :: RGB
    mempty = RGB (0, 0, 0)

--  mappend :: RGB -> RGB -> RGB
    mappend (RGB (r1, g1, b1)) (RGB (r2, g2, b2)) = RGB (r, g, b)
        where
             r = min (r1 + r2) 255
             g = min (g1 + g2) 255
             b = min (b1 + b2) 255

-- ist subtraktive Farbmischung ein Monoid für den Typ RGB?
--
-- A: Nein, da die Subtraktion nicht assoziativ ist
--    Gegenbeispiel:  (1 - 2) - 3 = -4 /= 2 = 1 - (2 - 3)

