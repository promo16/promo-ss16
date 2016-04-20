module Warteschlange (leer, einstellen, abholen, fanwerden) where

data Warteschlange a = WS [a] [a]

leer :: Warteschlange a
leer = WS [] []

einstellen :: a -> Warteschlange a -> Warteschlange a
einstellen x (WS ein aus) = WS (x:ein) aus

abholen :: Warteschlange a -> (Maybe a, Warteschlange a)
abholen (WS ein (x:aus)) = (Just x, WS ein aus)
abholen (WS []  []     ) = (Nothing,      leer)
abholen (WS ein []     ) = abholen (WS [] (reverse ein))

fanwerden :: (a -> b) -> Warteschlange a -> Warteschlange b
fanwerden f (WS ein aus) = WS (map f ein) (map f aus)

instance Functor Warteschlange where
    fmap f (WS ein aus) = WS (map f ein) (map f aus)

instance Show a => Show (Warteschlange a) where
    show (WS ein aus) = "WS" ++ show (ein, aus)
