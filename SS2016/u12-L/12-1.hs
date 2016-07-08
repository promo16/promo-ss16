module Bank (
    withdraw
  , deposit
  , accountState
  ) where

import Data.List (sort)

-- 12-1 b) ^

-- | Funktionale Fehlerbehandlung und Module
--

-- Wir führen hier einen neuen Datentyp ein - 'Either'
--
-- Dieser ist stark verwandt mit 'Maybe', nur dass er statt dem 'Nothing' Konstruktor einen Konsturktor hat
-- der einen Wert annehmen kann

{-

data Either a b = Left  a
                | Right b

-}

-- Dieser wird oft zur Fehlerbehandlung genutzt, weil er entweder ein richtiges Ergebnis darstellt ('Right'),
-- oder eine Fehler passiert ist ('Left'). Der Unterschied zu 'Maybe' ist, dass 'Left' noch eine Fehlermeldung
-- in z.B Form eines Strings zurückgeben kann.

safeDivision :: Int -> Int -> Either String Int
safeDivision x 0 = Left "Fehler! Unsere mathematischen Konzepte erlauben uns nicht durch Null zu teilen"
safeDivision x y = Right (x `div` y)

-- λ> safeDivision 4 0
-- Left "Fehler! Unsere mathematischen Konzepte erlauben uns nicht durch Null zu teilen"
-- it :: Either String Int
--
-- λ> safeDivision 4 2
-- Right 2
-- it :: Either String Int
--

-- a) Erweitern sie das Programm in 11-1 um statt 'Maybe' nun 'Either' zu benutzen sodass folgende Regeln eingehalten werden:

--    *) Einzahlungen dürfen nur positiv sein
--    *) Auszahlungen dürfen nur negativ sein
--    *) Auszahlungen dürfen maximal 1000€ auf einmal betragen
--    *) Kredit darf maximal 1000€ betragen

type Money   = Int
type Balance = (Money, Money)
type Account = Money

withdraw :: Money -> Balance -> Either String Balance
withdraw money (debit, credit)
    | money >  0                                   = Left "Auszahlungen dürfen nur negativ sein"
    | money < (-1000)                              = Left "Auszahlungen dürfen maximal 1000€ auf einmal betragen"
    | debit + credit + money < (-1000)             = Left "Nope, soviel Geld hast du nicht"
    | otherwise                                    = Right (debit + money, credit)

deposit :: Money -> Balance -> Either String Balance
deposit money (debit, credit)
    | money < 0 = Left "Einzahlungen dürfen nur positiv sein"
    | otherwise = Right (debit, credit + money)


accountState :: Balance -> Either String Account
accountState (debit, credit) = Right (debit + credit)


-- b) Siehe erste Zeile

-- Zum Exportieren und Modulen gibt es nicht viel zu sagen:
--
-- *) Syntax - module MODULNAME (funktionA, funktionB, funktionC) where
-- *) Alle Funktionen die nicht genannt wurden sind von aussen nicht mehr sichtbar wenn man das Modul importiert (denke an 'private')
--
