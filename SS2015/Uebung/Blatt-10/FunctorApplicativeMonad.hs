module Main where

import Prelude hiding (Either(..))
import Control.Applicative
import Control.Monad (foldM)

-- Beispielimplementierung für unsere Darstellungzwecke

data Either a b = Left  a
                | Right b
  deriving (Show, Eq, Ord)


-- Hier einigen wir uns darauf, dass wenn der Konstruktor Right aufgerufen wird,
-- die Berechnung funktioniert!

-- Wenn der Left Konstruktor augerufen wird, ist irgendwas fehlgeschlagen und wir
-- brechen jede Berechnung ab! (d.h in jeden 'Left'-Fall wenden wir nichts mehr
-- auf das Argument von 'Left' an)



-- | Erklärung von Functor / Applicative / Monad

{- Stellen wir erstmal die Definitionen klar

  - einfacher Typ ist im folgenden z.B Int oder String

  - einfache Funktion ist im folgenden die nur mit einfachen Typen arbeitet
      z.B (+) :: Int -> Int -> Int

  - ein komplexer Typ ist im folgenden ein einfacher Typ der
    in einem selbst definierten Datentyp mit mehereren Konstruktoren
    versteckt ist - z.B Maybe Int hat die Konstruktoren 'Nothing' und 'Just Int'
                        Either String Int hat die Konstruktoren 'Left String' und 'Right Int'

  - ein Funktor ist ein komplexer Datentyp der eine Instanz von
    der Typklasse Functor ist. Diese implemetiert die Funktion:

      fmap :: Functor f => (a -> b) -> f a -> f b

    Was macht diese Funktion in Worten?

      - sie macht aus dem komplexen Datentyp 'f a' ein 'a'
      - sie wendet auf dieses 'a' die einfache Funktion '(a -> b)' an
      - sie verpackt das Ergebnis 'b' in den komplexen Datentyp
       => es kommt ein f b raus

    Man stelle sich das mit Boxen vor:

      - ich habe eine Funktion die nimmt ein  Blatt Papier und zündet es an
      - ich habe ein Blatt Papier, dass in einer Box liegt

      - bevor ich die Funktion auf das Blatt anwenden kann, muss ich es erst aus
        der Box holen und dann kann ich es anzünden -> es bleibt Asche übrig!

      - dann muss man natürlich die Asche in die Box zurücktun

    In diesem Fall waren die Boxen unser 'f', die Blätter waren unser 'a'
    und die Asche ist unser 'b'

    Definieren wir uns Functor für unseren Datentyp Either!
-}

instance Functor (Either a) where

--fmap :: (a -> b) -> Either c a -> Either c b
--        \______/     \_______/    \________/
--           /        /                 |
--          /        /                  |
--      ___/__   ___/__      ___________|___
--     /      \ /      \    /               \
  fmap function (Right a) = Right (function a)

  fmap function (Left  a) = Left a

-- Beispiel (einmal mit Infix-, einmal Prefix-Schreibweise)

f1 :: Either String Int
f1 =      (\x -> x * 2) `fmap` Left "hello"
--        (\x -> x * 2) `fmap` Left "hello"
--                             Left "hello"

f2 :: Either String Int
f2 = fmap (\x -> x * 2)       (Right 4)
--   fmap (\x -> x * 2)       (Right 4)
--                             Right (4*2)
--                             Right 8

f3 :: Either String (Int -> Int)
f3 = fmap (*) (Right 4)                     -- => Right (\x -> x * 4) (kann nicht im GHCI ausgewertet werden, da eine Funktion keine Show-Instanz hat)

f4 :: Either String ([a] -> [a])
f4 = fmap (++) (Left "Hi")                  -- => Left "Hi" (kann auch nicht ausgewertet werden, da GHCI glaubt, dass wir (++) verwenden
--                                                           werden - dabei schmeißen wir das Ergebnis später eh weg!)

{-
    - ein Applicative ist ein Datentyp der eine Instanz von der
    Typklasse Applicative ist. Diese implementiert die Funktionen:

    pure :: Applicative f => a -> f a

    In Worten:

      - wir tun unser Blatt in eine Box

    (<*>) :: Applicative f => f (a -> b) -> f a -> f b

    In Worten:

      - in der Box liegt bereits der Anzünder und in der zweiten Box liegt das Papier
      - wir holen beides aus den Boxen raus, zünden das Blatt an und tun die Asche wieder zurück in die Box


    Wir erinnern uns - wenn einmal der 'Left'-Konstruktor vorkommt, verwerfen wir jegliche Berechnung
    und geben dieses 'Left' zurück!

    Jetzt müssen wir uns noch darauf einigen, wenn wir 'pure'-Funktion aufrufen, dass das Ergebnis immer 'richtig' ist
    und wir es deswegen in den 'Right'-Konstruktor rein tun.

-}

instance Applicative (Either a) where

--      pure :: a -> Either b a
--          ____|    \________/
--         /      _______|
--        /   ___|_
--       |   |     |
    pure a = Right a

--      (<*>) :: Either c (a -> b) -> Either c a -> Either c b
--               |_______________|    \________/    \________/
--           ________|                    |             |
--          /                   __________|             |
--   ______|_______        ____|___    _________________|____
--  |              |      |        |  |                      |
    (Right function)  <*> something = fmap function something

--  Dieser Fall darf niemals passieren, da der Left-Fall nur dann passiert, wenn ein Fehler passiert
--  Falls es doch per Hand definiert wird - geben wir die Funktion einfach aus
    (Left  something) <*> _         =  Left something


a1 :: Either String Int
a1 = Right (\x -> x * 2) <*> Right 4
--   Right (\x -> x * 2) <*> Right 4
--                           Right (4*2)
--                           Right 8

a2 :: Either String Int
a2 = Right (\x -> x * 2) <*> Left "error"
--   Right (\x -> x * 2) <*> Left "error"
--                           Left "error"


-- | Dieser Ausdruck wertet wieder zu a1 aus!
a3 :: Either String Int
a3 = (*) <$> Right 2 <*> Right 4
--   (*) <$> Right 2             <*> Right 4
--           Right (\x -> x * 2) <*> Right 4
--                                   Right (4 * 2)
--                                   Right 8


a4 :: Either String Int
a4 = Right (\x -> x * 2) <*> Left "error"
--   Right (\x -> x * 2) <*> Left "error"
--                           Left "error"

a5 :: Either String Int
a5 = (*) <$> Right 4 <*> Left "error"
--   (*) <$> Right 4 <*> Left "error"
--           Right (\x -> x * 4) <*> Left "error"
--                                   Left "error"

a6 :: Either String Int
a6 = (*) <$> Left "hi" <*> Right 5
--   (*) <$> Left "hi" <*> Right 5
--           Left "hi" <*> Right 5
--                         Left "hi"

{-  Aber was ist hier passiert? Ist das nicht eigentlich ein Fehler?

    Im Typ von <*> stand, dass das erste Argument (das, was links davon steht), muss eine Funktion sein.

    In diesem Ausdruck:

    Left "hi" <*> Right 5

    Left "hi" ist offensichtlich keine Funktion - wieso wirft das keinen Typfehler?

    Schaun wir uns den Typ von dem Ausdruck an

    :t ( Left "hi" <*> ) :: Either String a    -> Either String a

    Hier ist es egal was das zweite Argument ist - es kommt immer (Left "hi") raus!
    Da Left "hi" aber vom Typ (Either String a) ist, kann das zweite Argument ein (a -> b) sein.

    Deswegen können wir erst <*> darauf anwenden und es gibt uns keinen Fehler raus, weil es eine Funktion erwartet!

    :t ( <*> Right 5 )   :: Either a1 (Int -> b) -> Either a1 b

    Hier haben wir zwei Möglichkeiten - Entweder das erste Argument ist ein
    (Right function) wobei function vom Typ (Int -> b) sein muss. Dann wird die
    '5' in diese Funktion gesetzt und wir geben ein (Right (irgendwas vom Typ b)) zurück.

    Da der zweite Typ von Either eine Funktion ist, wertet dieser Ausdruck überhaupt erst aus.

    Falls es aber ein (Left x) ist, wird das zweite Argument verworfen und das Left wird zurückgegeben!

-}


{-  - eine Monade ist ein Datentyp der eine Instanz von der
    Typklasse Monade ist. Diese implementiert die Funktionen:

    return :: Monad m => a -> m a

    In Worten:

      - wir tun unser Blatt in eine Box (analog zu 'pure')

    (>>=) :: Monad m => m a -> (a -> m b) -> m b

    In Worten:

      - wir nehmen eine Box und holen das Papier raus
      - wir haben eine Funktion die ein Blatt nimmt, es anzündet, zu Asche macht
        und automatisch in eine Box packt, ohne das die Monade was machen muss!
      - dann führen wir diese Funktion auf das Papier aus

-}

instance Monad (Either a) where


--    return :: a -> Either b a
      return a = Right a
  --  return a = pure      Hier ist die Verbindung zu Applicative (analoge Definition von return)


      Right a >>= function = function a
      Left a  >>= _        = Left a

muu :: Int -> Int -> Either String Int
muu x 0 = Left "error"
muu x y = Right (x `div` y)


m1 :: Either String Int
m1 = muu 6 2 >>= \result -> return result
--   muu 6 2 >>= \result -> return result
--   Right 3 >>= \result -> return result
--               \3 -> return 3
--                     return 3
--                     Right  3

m1' :: Either String Int
m1' = do
  result <- muu 6 2
  return result

m2 :: Either String Int
m2 = muu 6 0 >>= \result -> return result
--   muu 6 0 >>= \result -> return result
--   Left "error" >>= \result -> return result
--   Left "error" >>= _
--                    Left "error"

m2' :: Either String Int
m2' = do
  result <- muu 6 0
  return result


m3 :: Either String [Int]
m3 = muu 6 2 >>= \r1 -> muu 6 1 >>= \r2 -> return [r1, r2]
--   muu 6 2 >>= \r1 -> muu 6 1 >>= \r2 -> return [r1, r2]
--   Right 3 >>= \r1 -> muu 6 1 >>= \r2 -> return [r1, r2]
--                      muu 6 1 >>= \r2 -> return [3, r2]
--                      Right 6 >>= \r2 -> return [3, r2]
--                                         return [3, 6]
--                                         Right  [3, 6]

m3' :: Either String [Int]
m3' = do
  r1 <- muu 6 2
  r2 <- muu 6 1
  return [r1,r2]


m4 :: Either String [Int]
m4 = muu 6 2 >>= \r1 -> muu 6 0      >>= \r2 -> return [r1, r2]
--   muu 6 2 >>= \r1 -> muu 6 0      >>= \r2 -> return [r1, r2]
--   Right 3 >>= \r1 -> muu 6 0      >>= \r2 -> return [r1, r2]
--                      muu 6 0      >>= \r2 -> return [3, r2]
--                      Left "error" >>= \r2 -> return [3, r2]
--                      Left "error" >>= _
--                                       Left "error"

m5 :: Either String Int
m5 = muu 20 2 >>= \r1 -> muu r1 5 >>= \r2 -> return r2
--   muu 20 2 >>= \r1 -> muu r1 5 >>= \r2 -> return r2
--   Right 10 >>= \r1 -> muu r1 5 >>= \r2 -> return r2
--                       muu 10 5 >>= \r2 -> return r2
--                       Right 2  >>= \r2 -> return r2
--                                           return 2
--                                           Right 2

m5' :: Either String Int
m5' = do
  r1 <- muu 20 2
  r2 <- muu r1 5
  return r2
--    => Right 2


-- analog zu m5 und m5'
m5'' :: Either String Int
m5'' = foldM muu 20 [2, 5]
--    => Right 2

m6 :: Either String Int
m6 = muu 20 2 >>= \r1 -> muu r1 0     >>= \r2 -> return r2
--   muu 20 2 >>= \r1 -> muu r1 0     >>= \r2 -> return r2
--   Right 10 >>= \r1 -> muu r1 0     >>= \r2 -> return r2
--                       muu 10 0     >>= \r2 -> return r2
--                       Left "error" >>= \r2 -> return r2
--                       Left "error" >>= _
--                                        Left "error"

m6' :: Either String Int
m6' = do
  r1 <- muu 20 2
  r2 <- muu r1 0
  return r2
--    => Left "error"


m6'' :: Either String Int
m6'' = foldM muu 20 [2, 0, 5]
--    => Left "error"