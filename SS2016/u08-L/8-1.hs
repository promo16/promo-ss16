
-- Aufgabe 8-1 Datentypen und Typklassen
--

-- a) Implementieren sie Datentypen 'Suit', 'Value' und 'Card' für ein Kartendeck aus 52 Karten
--
data Suit = Clubs
          | Hearts
          | Spades
          | Diamonds
    deriving (Eq, Enum, Show, Ord)         -- b) Definieren sie "nützliche" Instanzen (mehr eine Aufgabe die durch c) und d) verständlich wird)

-- *) Wir brauchen Eq, weil wir die Gleichheit von Karten bestimmen wollen
-- *) Wir brauchen Enum, weil wir dann [Clubs .. Diamonds] schreiben wollen
-- *) Wir brauchen Show für die Ausgabe (aber eher unwichtig an dieser Stelle)

data Value = Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
           | Ace
    deriving (Eq, Ord, Enum, Show)   -- b)

-- *) Wir brachen Eq, weil wir die Karten vergleichen wollen
-- *) Wir brauchen Ord, weil wir die Ord Instanz für Karten definieren und diese leitet uns automatisch die Wertigkeit
--    der Karten ab (Two < Three < .. < Ace)
-- *) Wir brauchen Enum, weil wir [Two .. Ace] schreiben wollen
-- *) Wir brauchen Show für die Ausgabe (eher unwichtig)

data Card = Value `Of` Suit      -- data Card = Of Value Suit   (analog)
    deriving (Show)              -- b)

-- *) Hier habe ich einen Konstruktor infix gesetzt, damit man es mal gesehen hat - des öfteren vereinfacht das die Lesbarkeit
-- *) Show wird hier automatisch durch die Show Instanzen von Value und Suit abgeleitet

-- c) Hearts sticht alles, ansonsten wird die Wertigkeit der Karte verglichen
--
-- Um Doppeldeutigkeit zu vermeiden definieren wir Eq through Ord:
--
instance Eq Card where

    (==) c1 c2 = compare c1 c2 == EQ

instance Ord Card where

    compare (val1 `Of` Hearts) (val2 `Of` Hearts) = compare val1 val2
    compare (val1 `Of` Hearts) (val2 `Of`      _) = GT
    compare (val1 `Of`      _) (val2 `Of` Hearts) = LT
    compare (val1 `Of`      _) (val2 `Of`      _) = compare val1 val2


-- Wer an dieser Stelle einfach Ord ableitet kriegt eine Ordnung nach der Reihenfolge
-- wie die Konstruktoren in 'Card' stehen. In diesem Fall kommt 'Value' vor 'Suit' und
-- deswegen sticht zuerst die Zahl und falls diese gleich ist, kommen die
-- Suits nach ihrer Definitionsreihenfolge.

-- > Nine `Of` Clubs `compare` Nine `Of` Spades
-- LT     (es sollte EQ sein)

-- > Seven `Of` Hearts `compare` Ten `Of` Diamonds
-- LT     (es sollte GQ sein)

-- Das ist natürlich nicht gewollt und deswegen muss man die Instanz selber definieren

-- Um die Reihenfolge zu überprüfen, kann man sehr einfach Data.List importieren
-- und sich die sortierte Liste ausgeben.

-- λ> :m +Data.List
-- λ> mapM_ print $ sort cards
-- Two `Of` Clubs
-- Two `Of` Spades
-- ...

-- d) Erstelle alle Karte in einem Deck
--
cards :: [Card]
cards = [val `Of` suit | suit <- [Clubs .. Diamonds], val <- [Two .. Ace]]