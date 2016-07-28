-- Aufgabe 10 - 3: Funktoren und Applikative

import Prelude hiding (fst, snd, product)

-- a) Implementieren sie einen Datentyp Triple

data Triple a = Triple { x :: a, y :: a, z :: a }

instance Show a => Show (Triple a) where

--  show :: Triple a -> String
    show (Triple x y z) = "(" ++ show x ++ ',' : show y ++ ',' : show z ++ ")"

-- b) Implementieren sie fst, snd, trd

fst :: Triple a -> a
fst (Triple x _ _) = x

snd :: Triple a -> a
snd (Triple _ y _) = y

trd :: Triple a -> a
trd (Triple _ _ z) = z

-- Durch RecordSyntax kriegen wir die 'getter' geschenkt
fst' :: Triple a -> a
fst' triple = x triple

snd' :: Triple a -> a
snd' triple = y triple

trd' :: Triple a -> a
trd' triple = z triple


-- c) tripToList, listToTriple

listToTriple :: [a] -> Triple a
listToTriple (x:y:z:_) = Triple x y z
listToTriple _         = error "10-3.listToTriple: incoming list has not enough elements"

tripToList :: Triple a -> [a]
tripToList (Triple x y z) = [x, y, z]

-- d) Kreuzprodukt

cross :: Num a => Triple a -> Triple a -> Triple a
cross (Triple a b c) (Triple d e f) = Triple { x = b * f - c * e
                                             , y = c * d - a * f
                                             , z = a * e - b * d
                                             }

-- Ohne Records
cross' :: Num a => Triple a -> Triple a -> Triple a
cross' (Triple a b c) (Triple d e f) = Triple (b * f - c * e) (c * d - a * f) (a * e - b * d)


-- e) scalar durch Functor

instance Functor Triple where

--  fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Triple x y z) = Triple (f x) (f y) (f z)


scalar :: Num a => a -> Triple a -> Triple a
scalar s triple = fmap (*s) triple

-- f) Skalarprodukt, Addition, Subtraktion (mit Applicative geht es etwas einfacher)

instance Applicative Triple where
    
--  pure :: a -> Triple a
    pure x = Triple x x x

--  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (<*>) (Triple fx fy fz) (Triple x y z) = (Triple (fx x) (fy y) (fz z))

product :: Num a => Triple a -> Triple a -> a
product a b = sum $ tripToList $ fmap (*) a <*> b

-- alternative Lösung ohne Applicative
product' (Triple a b c) (Triple d e f) = a*d + b*e + c*f

addition :: Num a => Triple a -> Triple a -> Triple a
addition a b    = fmap (+) a <*> b

-- alternative Lösung ohne Applicative
addition' (Triple a b c) (Triple d e f) = Triple (a + d) (b + e) (c + f)

subtraction :: Num a => Triple a -> Triple a -> Triple a
subtraction a b = fmap (-) a <*> b

-- alternative Lösung ohne Applicative
subtraction' (Triple a b c) (Triple d e f) = Triple (a - d) (b - e) (c - f)


-- g) Länge des Vektors

len :: Floating a => Triple a -> a
len a = sqrt (product a a)