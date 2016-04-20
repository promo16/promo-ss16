module Main where


-- | A3-3

-- a)
--   Typname    Konstruktor  Variablen
data Akteur   = Spieler      String Double
              | Computer     Int
              | Zuschauer    String

-- | Typnamen und Konstruktoren sind einzigartig und
--   fangen immer mit einem Großbuchstaben an


-- b)

anzeige :: Akteur -> String
anzeige (Spieler   name      _) = name
anzeige (Computer  strength   ) = "KI(" ++ show strength ++ ")"
anzeige (Zuschauer name       ) = '[' : name ++ "]"


n_1 = anzeige $ Spieler "Peter" 20.0
n_2 = anzeige $ Computer 5
n_3 = anzeige $ Zuschauer "Hans"


-- | Man kann sich auch die 'show'-Funktion von Haskell
--   erstellen lassen (mit deriving), was jedoch zu einer nicht so schönen Ausgabe führt

data Akteurin = Spielerin     String Double
              | Computerin    Int
              | Zuschauerin   String
    deriving Show

m_1 = show $  Spielerin "Laura" 20.0
m_2 = show $  Computerin 5
m_3 = show $  Zuschauerin "Maria"


-- | Oder man definiert die 'Show' Instanz selber
instance Show Akteur where
  show (Spieler   name      _) = name
  show (Computer  strength   ) = "KI(" ++ show strength ++ ")"
  show (Zuschauer name       ) = '[' : name ++ "]"


k_1 = show $ Spieler "Peter" 20.0
k_2 = show $ Computer 5
k_3 = show $ Zuschauer "Hans"


main :: IO()
main = do
  putStrLn "\nAnzeige:\n"
  putStrLn $ n_1 ++ " | " ++ n_2 ++ " | " ++ n_3
  putStrLn "\nDeriving Show:\n"
  putStrLn $ m_1 ++ " | " ++ m_2 ++ " | " ++ m_3
  putStrLn "\nEigene Show Instanz:\n"
  putStrLn $ k_1 ++ " | " ++ k_2 ++ " | " ++ k_3