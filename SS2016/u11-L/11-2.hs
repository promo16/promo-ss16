-- | Aufgabe 11 - 2 - Retrievable
--
--
-- a) + b) + c) + d) definieren sie den Datentyp 'Retrievable'

data Retrievable a = Present a
                   | NotAvailable
    deriving Show


-- | Erste Variante - wir definieren zuerst Functor -> Applicative -> Monad

instance Functor Retrievable where

--  fmap :: (a -> b) -> Retrievable a -> Retrievable b
    fmap f (Present x)  = Present (f x)
    fmap f NotAvailable = NotAvailable

instance Applicative Retrievable where

--  pure :: Retrievable a
    pure x = Present x

--  (<*>) :: Retrievable (a -> b) -> Retrievable a -> Retrievable b
    Present f <*> Present x = Present (f x)
--  Present f <*> mx        = fmap f mx         (analog zur Zeile 26)
    _         <*> _         = NotAvailable

instance Monad Retrievable where

--  return :: Retrievable a
    return x = pure x

--  (>>=) :: Retrievable a -> (a -> Retrievable b) -> Retrievable b
    (Present x)  >>= f = f x
    NotAvailable >>= _ = NotAvailable




-- | Zweite Variante - wir definieren zuerst Monad -> Applicative -> Functor

data Vielleicht a = Yep a
                  | Nope
    deriving Show

instance Monad Vielleicht where

--  return :: Vielleicht a
    return x = Yep x

--  (>>=) :: Vielleicht a -> (a -> Vielleicht b) -> Vielleicht b
    (Yep x) >>= f = f x
    Nope    >>= _ = Nope


instance Applicative Vielleicht where

--  pure :: Vielleicht a
    pure x = return x

--  (<*>) :: Vielleicht (a -> b) -> Vielleicht a -> Vielleicht b
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

--  oder analog

--  (<*>) :: Vielleicht (a -> b) -> Vielleicht a -> Vielleicht b
--    mf <*> mx = do
--        f <- mf
--        fmap f mx

instance Functor Vielleicht where

--  fmap :: (a -> b) -> Vielleicht a -> Vielleicht b
    fmap f mx = do
        x <- mx
        return (f x)

--  oder analog

--  fmap :: (a -> b) -> Vielleicht a -> Vielleicht b
--    fmap f mx = pure f <*> mx



-- Weitere Erklärungen und Beispiele zu Monaden / Applicative / Functor sind in 10-2 und 11-1
--
-- Übungen zu den Themen:
--     Implementiere aus dem Kopf
--          *) Maybe (Eq, Ord, Functor, Applicative, Monad, Monoid)
--          *) []    (Eq, Ord, Functor, Applicative, Monad, Monoid)
--          *) Binärbaum (Eq, Ord, Functor)
--          *) Either (Eq, Ord, Functor, Applicative, Monad)
--
-- Bei Fragen dazu einfach schreiben, oder hooglen











-- Mögliche Lösungen (hier ist immer die Herleitung Functor -> Applicative -> Monad, weil diese meistens schwerer ist):
--


-- | Maybe (die Lösung zu Functor, Applicative, Monad steht bereits oben)
--
instance (Eq a) => Eq (Vielleicht a) where

    (==) (Yep a1) (Yep a2) = a1 == a2
    (==)  Nope     Nope    = True
    (==)  _        _       = False


--  Ein enthaltener Wert 'zählt' mehr als kein Wert => (Yep x) > Nope
--
instance (Ord a) => Ord (Vielleicht a) where

    compare (Yep a1) (Yep a2) = compare a1 a2
    compare  Nope     Nope    = EQ
    compare (Yep  _)  Nope    = GT
    compare  Nope    (Yep  _) = LT


--  wir brauchen hier die Einschränkung dass der Typ in dem Maybe auch eine Monoidinstanz hat,
--  damit wir sie irgendwie verknüpfen können
--
instance (Monoid a) => Monoid (Vielleicht a) where

--  mempty :: Vielleicht a
    mempty = Nope

--  mappend :: Vielleicht a -> Vielleicht a -> Vielleicht a
    mappend (Yep a1) (Yep a2) = Yep (a1 `mappend` a2)
    mappend (Yep a1)  Nope    = Yep a1
    mappend  Nope    (Yep a2) = Yep a2
    mappend  Nope     Nope    = Nope






-- | Linked List
--
data List a = Node a (List a)
            | End
    deriving Show

instance (Eq a) => Eq (List a) where

    (==) (Node x xs) (Node y ys) = x == y && xs == ys
    (==)  _           _          = False

instance (Ord a) => Ord (List a) where

    compare (Node x xs) (Node y ys)
        | compare x y == EQ = compare xs ys
        | otherwise         = compare x y

    compare (Node _ _)   End       = GT
    compare  End        (Node _ _) = LT
    compare  End         End       = EQ

instance Monoid (List a) where

--  mempty :: List a
    mempty  = End

--  mappend :: List a -> List a -> List a
    mappend  End        xs = xs
    mappend (Node a as) xs = Node a (as `mappend` xs)

instance Functor List where

--  fmap :: (a -> b) -> List a -> List b
    fmap _  End        = End
    fmap f (Node a as) = Node (f a) (fmap f as)

instance Applicative List where

--  pure :: a -> List a
    pure a = Node a End

--  (<*>) :: List (a -> b) -> List a -> List b
    (<*>) (Node f fs) as = fmap f as `mappend` (fs <*> as)
    (<*>)  End        _  = End

instance Monad List where

--  return :: a -> List a
    return a = Node a End

--  (>>=) :: List a -> (a -> List b) -> List b
    (>>=) (Node a as) f = f a `mappend` (as >>= f)
    (>>=)  End        _ = End







-- | Binarytree
--
data BTree a = BNode a (BTree a) (BTree a)
             | Leaf
    deriving Show

instance (Eq a) => Eq (BTree a) where

    (==) (BNode a1 left1 right1) (BNode a2 left2 right2) = a1 == a2 && left1 == left2 && right1 == right2
    (==)  Leaf                    Leaf                   = True
    (==)  _                       _                      = False


-- instance (Ord a) => Ord (BTree a) where
-- 
--     compare (BNode a1 left1 right1) (BNode a2 left2 right2)
--             | cmpValue    /= EQ = cmpValue
--             | cmpLeftTree /= EQ = cmpLeftTree
--             | otherwise         = cmpRightTree
--         where cmpValue     = compare a1 a2
--               cmpLeftTree  = compare left1 left2
--               cmpRightTree = compare right1 right2
--     compare (BNode _ _ _)  Leaf         = GT
--     compare  Leaf         (BNode _ _ _) = LT
--     compare  Leaf          Leaf         = EQ

-- Man sieht das diese Instanz nun wirklich nicht hübsch ist. Bloß gut dass wir Ord auch durch '<=' definieren können

instance (Ord a) => Ord (BTree a) where

    (<=)  Leaf             Leaf            = True
    (<=) (BNode _  _  _)   Leaf            = False
    (<=)  Leaf            (BNode _  _  _)  = True
    (<=) (BNode a1 l1 r1) (BNode a2 l2 r2) = a1 == a2 && l1 == l2 && r1 <= r2  -- Wert am Knoten und linke Teilbäume sind gleich -> wir vergleichen den rechten Teilbaum
                                          || a1 == a2 && l1 <= l2              -- Wert am Knoten ist gleich -> wir vergleichen den linken Teilbaum
                                          || a1 <= a2                          -- Wert am Knoten wird verglichen

-- Trotzdem etwas unschön und eventuell auf Anhieb nicht verständlich ob es richtig ist.
-- Am besten eine 'toList :: BTree a -> [a]' Funktion schreiben und dann die Listen vergleichen,
-- oder noch besser - einfach gleich deriven.

instance Functor BTree where

--  fmap :: (a -> b) -> BTree a -> BTree b
    fmap _ Leaf                 = Leaf
    fmap f (BNode a left right) = BNode (f a) (fmap f left) (fmap f right)






-- | Either
--
data MalSchauen a b = Nagut a
                    | Passt b
    deriving Show

-- | Hier analog zur Maybe-Instanz die Einschränkung für (Monoid b)
--
--   Die Motivation hinter dieser Implementierung ist, dass jedes Mal wenn 'Nagut' vorkommt,
--   ein Fehler eingetreten ist und die Berechnung nicht weitergehen sollte. (Genauso wie bei 'Either')
--    
instance (Eq a, Eq b) => Eq (MalSchauen a b) where

    (==) (Nagut a1) (Nagut a2) = a1 == a2
    (==) (Passt b1) (Passt b2) = b1 == b2
    (==)  _          _         = False

--  'Passt' "wiegt" mehr (Passt x > Nagut x)
instance (Ord a, Ord b) => Ord (MalSchauen a b) where

    compare (Nagut a1) (Nagut a2) = compare a1 a2
    compare (Passt b1) (Passt b2) = compare b1 b2
    compare (Passt _ )  _         = GT
    compare  _         (Passt _ ) = LT

--
--   Ich geben hier lediglich dann 'Passt' zurück, wenn beide Argumente diesen Konstruktor haben
--
instance (Monoid b) => Monoid (MalSchauen a b) where

--  mempty :: MalSchauen a b
    mempty = Passt mempty

--  mappend :: MalSchauen a b -> MalSchauen a b -> MalSchauen a b
    mappend (Nagut a)  _          = Nagut a
    mappend  _         (Nagut a)  = Nagut a
    mappend (Passt b1) (Passt b2) = Passt (b1 `mappend` b2)


instance Functor (MalSchauen c) where

--  fmap :: (a -> b) -> MalSchauen c a -> MalSchauen c b
    fmap f (Passt a) = Passt (f a)
    fmap f (Nagut c) = Nagut c

instance Applicative (MalSchauen c) where

--  pure :: a -> MalSchauen c a
    pure a = Passt a

--  (<*>) :: MalSchauen c (a -> b) -> MalSchauen c a -> MalSchauen c b
    (<*>) (Passt f) (Passt a) = Passt (f a)
    (<*>) (Nagut c)  _        = Nagut c
    (<*>)  _        (Nagut c) = Nagut c

instance Monad (MalSchauen c) where

--  return :: a -> MalSchauen c a
    return a = Passt a

--  (>>=) :: MalSchauen c a -> (a -> MalSchauen c b) -> MalSchauen c b
    (>>=) (Passt a) f = f a
    (>>=) (Nagut c) _ = Nagut c

-- Wer auf Anhieb diese Definition nicht nachvollziehen kann, es braucht immer etwas Zeit bis man Instanzen für Datentypen
-- mit mehreren Typvariablen schreiben kann. Ich habe versucht die Variablen immer genauso zu bennen, wie der Typ den sie haben.

-- Ich glaube der 'Klick' bei mir war damals, wo ich verstanden habe dass z.B
--
-- > Passt 1 :: MalSchauen a Int
--
-- von diesem Typ ist. Das heißt der linke Konstruktor hat keinerlei Einschränkung, welchen Typ er annehmen kann.

-- > Passt 1 :: MalSchauen String      Int
-- > Passt 1 :: MalSchauen Int         Int
-- > Passt 1 :: MalSchauen [Maybe Int] Int

-- genauso umgekehrt mit dem 'Nagut' - Konstruktor

-- > Nagut 1 :: MalSchauen Int a
-- > Nagut 1 :: MalSchauen Int String
-- > Nagut 1 :: MalSchauen Int (Maybe Int)
-- > Nagut 1 :: MalSchauen Int (MalSchauen Int Int)

-- Deswegen typechecken die Zeilen 223,224 und 233.

-- Ich habe dieses Beispiel lediglich deswegen reingepackt, weil wir in der letzten Vorlesung 'Either' behandelt haben.
-- Es ist aber eher unwahrscheinlich, dass so eine schwierige Aufgabe drankommt.


-- Um nochmal zu wiederholen was auf 11-1 stand - wer mit Maybe Functor-Applicative-Monad implementieren kann, kann
-- sich perfekt vorbereitet für die Klausur sehen.