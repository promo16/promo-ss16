module Bonus (eintragAbschreiber, eintragPunkte, punkteAuslesen, leer) where

import qualified Data.Map as Map

type Map      = Map.Map
type Register = Map Student Bonus
type Student  = String
data Bonus    = Punkte [Int] | Abschreiber


instance Show Bonus where
  show (Punkte l)    = "hat " ++ show (sum l) ++ " Punkte!"
  show (Abschreiber) = "ist ein Abschreiber!"

-- Dokumentation zu Bibliotheken

-- https://www.haskell.org/hoogle/?hoogle=Data.Map.Strict


leer :: Register
leer = Map.empty


punkteAuslesen :: Student -> Register -> Int
punkteAuslesen student register = case Map.lookup student register of
    (Just (Punkte punkte)) -> sum punkte
    (Just Abschreiber)     -> 0 --  \
    Nothing                -> 0 -- -- Beide Fälle kann man mit (_) -> 0 abfangen


eintragAbschreiber :: Student -> Register -> Register
eintragAbschreiber student register = Map.insert student Abschreiber register


eintragPunkte :: Student -> Int -> Register -> Register
eintragPunkte student pts register = case Map.lookup student register of
    (Just (Punkte punkte)) -> Map.insert student (Punkte (pts:punkte)) register
    (Just Abschreiber)     -> register
    Nothing                -> Map.insert student (Punkte [pts]) register

-- Beispiele:

reg_1 =   eintragPunkte "Hans"  2
        $ eintragPunkte "Laura" 4
        $ eintragPunkte "Hans"  3 leer

reg_2 =   eintragAbschreiber "Robert"
        $ eintragPunkte "Mia"    3
        $ eintragPunkte "Robert" 5 leer

reg_3 =   punkteAuslesen "Anna"
        $ eintragPunkte "Anna" 8
        $ eintragAbschreiber "Florian"
        $ eintragPunkte "Anna" 5 leer

reg_4 =   punkteAuslesen "Nadia"
        $ eintragPunkte "Klaus" 2
        $ eintragAbschreiber "Nadia" leer