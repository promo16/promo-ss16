-- Aufgabe 6-5
--
-- Benutzer-definierte Typen
--
--
-- | Syntax für eigene Datentypen
--
-- data <Typname> = <Konstruktor1> [<arg1>, <arg2>,...]
--                | <Konstruktor2> [<arg1>, <arg2>,...]
--                | ...
--
-- | Fakten:
--
-- * Typname sowie Konstruktoren fangen mit einem Großbuchstaben an
-- * die Argumente, falls sie benannt sind, fangen mit Kleinbuchstaben an (es sind normale Funktionen)
-- * Typname und Konstruktor können gleich heißen

-- | Beispiele:
data MBool = MTrue
           | MFalse
    deriving Show

data MMaybe = Sure
            | Nope
            | Maybe

-- | Was bedeutet deriving?
--
-- Ihr kennt nun Typklassen wie 'Ord', 'Eq', 'Show' etc.
-- Man kann diese Instanzen per Hand definieren (kommt später.), oder man kann sie sich automatisch generieren lassen,
-- so gut wie Haskell es einfach schafft. 'Ord' z.B nummeriert einfach von 0 aufwärst die Konstruktoren nach der Reihenfolge.

data Test = Test | Klausur
  deriving (Show, Eq, Ord)

-- > :set +t
-- > let x = Test
-- > x
-- Test
-- it :: Test
-- 
--
-- > x == x
-- True
-- it :: Bool
--
-- > Test < Klausur
-- True
-- it :: Bool

-- Aufpassen 'Ord' braucht auch eine 'Eq' Instanz als Voraussetzung! (hogglen, wenn ihr diese Voraussetzungen wissen wollt)

-- a) Datentyp für Wochentage definieren
--

data Wochentag = Montag
               | Dienstag
               | Mittwoch
               | Donnerstag
               | Freitag
               | Samstag
               | Sonntag
  deriving (Show)

-- b) Erstellen sie eine Funktion 'gestern :: Wochentag -> Wochentag'

gestern :: Wochentag -> Wochentag
gestern Montag     = Sonntag
gestern Dienstag   = Montag
gestern Mittwoch   = Dienstag
gestern Donnerstag = Mittwoch
gestern Freitag    = Donnerstag
gestern Samstag    = Freitag
gestern Sonntag    = Samstag

-- c) Erstellen sie eine Funktion 'istEndlicheWochenende :: Wochentag -> Bool'

istEndlicheWochenende :: Wochentag -> Bool
istEndlicheWochenende Samstag = True
istEndlicheWochenende Sonntag = True
istEndlicheWochenende _       = False