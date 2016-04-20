module Main where

-- | H9-1

main :: IO ()
main = do

-- DO-Notation
  --do_loop []

-- Bind-Notation
  bind_loop []


-- | DO - Notation

-- Bei der DO-Notation darf man einfach alle monadischen Aktionen untereinander schreiben
-- Man sollte aber beachten, dass es nur eine syntactic sugar ist für die bind-Notation
-- d.h der GHC wandelt diesen Code automatisch in die Bind-Notation um!


do_loop :: [String] -> IO ()
do_loop list = do

-- Ausgabe der Anleitung
  putStrLn $ "Hi! Gib bitte zuerst dein Lieblingstier und dann in "
          ++ "die nächste Zeile deine Lieblingseigenschaft ein:"

-- Abfrage der ersten Eingabe - speichere das Ergebnis in 'tier'
  tier        <- getLine

-- Abfrage der zweiten Eingabe - speichere das Ergebnis in 'eigenschaft'
  eigenschaft <- getLine

-- Überprüfung, welche der Eingaben leer ist
  case (tier, eigenschaft) of

-- beide sind leer => schreib Spielverderber und ruf dich rekursiv
-- mit der Eigenschafts-Liste auf
      ([], []) -> do putStrLn "Spielverderber!"
                     do_loop list

-- nur tier ist leer => schreib Tier eingebene und ruf dich rekursiv
-- mit der Eigenschafts-Liste auf, wobei noch die aktuelle Eigenschaft
-- von vorne drangehängt wird
      ([], _ ) -> do putStrLn "Tier eingeben!"
                     do_loop (eigenschaft : list)

-- beide sind nicht leer => schreib das Endergebnis formatiert hin!
      (_ , _ ) -> do putStrLn $ "Psst willst Du "
                          ++ unwords (eigenschaft : list)
                          ++ ' ' : tier
                          ++ " kaufen?"

-- unwords ["Hallo", "ich", "heiße", "Paul", "!"]
-- => "Hallo ich heiße Paul !"
-- => es fügt alle Strings zu einem zusammen und schiebt
--    zwischen sie ein Leerzeichen ein!


-- Bind-Notation
-- Dieser Code ist in gewohnter prozeduralen Schreibweise hingeschrieben!
bind_loop :: [String] -> IO ()
bind_loop list = putStrLn ("Hi! Gib bitte zuerst dein Lieblingstier und dann in "
                       ++ "die nächste Zeile deine Lieblingseingeschaft ein:") >>
                 getLine >>= \tier ->
                 getLine >>= \eigenschaft ->
                 case (tier, eigenschaft) of
                    ([], []) -> putStrLn "Spielverderber" >> bind_loop list
                    ([], _ ) -> putStrLn "Tier eingeben!" >> bind_loop (eigenschaft : list)
                    ( _, _ ) -> putStrLn $ "Psst, willst Du "
                                      ++ unwords (eigenschaft : list)
                                      ++ ' ' : tier
                                      ++ " kaufen?"


-- | H9-2


  -- gegeben:

  --   - case e_1 of { Nothing -> e_2; (Just x) -> e_3 } :: A

  --     oder anders geschrieben:

  --     case e_1 of
  --       Nothing  -> e_2 :: A
  --       (Just x) -> e_3 :: A


  -- gefragt:

  --   - was können wir aus diesem Ausdruck für Typregeln herleiten?
  --     anders gesagt: Welchen Typ hat e_1, e_2, e_3 und x - und wieso?
  --   - formale Schreibweise von dieser Typregel

  -- Sei K der Kontext


  -- 1) Da wir im case Ausdruck gegen die 'Just'- und 'Nothing'-Konstruktoren
  --    matchen, muss e_1 vom Typ 'Maybe B' sein!

  --    'Maybe' wissen wir wegen den Konstruktoren, 'B' ist die nächste Typvariable
  --    nach dem bereits benutzten 'A'! (Kann auch anders benannt werden!)

  -- 2) Da wir bereits gegeben haben, dass e_2 und e_3 vom Typ 'A' ist, schreiben wir
  --    es einfach auf!

  -- 3) Wir schreiben wieder e_3 auf, da wir wissen dass der Typ 'A' ist! Zusätzlich
  --    ist aber eine neue freie Variable vorhanden - die müssen wir in den Kontext aufnehmen.
  --    Den Typ wissen wir aus 1), da wir ihn dort 'Maybe B' genannt haben, muss 'x' vom Typ 'B' sein.


  --           1)                           2)                          3)
  --    _______|_________              _____|_____             _________|_________
  --   /                 \            /           \           /                   \
  --   K |- e_1 :: Maybe B            K |- e_2 :: A           K, x :: B |- e_3 :: A
  -- _________________________________________________________________________________   (Case)
  --             K |- case e_1 of { Nothing -> e_2; Just x -> e_3 } :: A


-- Als gute Vorbereitung für die Klausur würde ich hier dazu anhalten für alle möglichen
-- Ausdrücke Typregeln zu erstellen (in der letzten Klausur war das eine "geschenkte" Aufgabe)

-- z.B

--  1. x:xs :: [a]        (Cons)
--  2. x ++ xs :: [a]     (Append)
--  3. foldl x y z :: d   (Foldl)
--  4. (x,y) :: (a, b)    (Tup)


-- | H9-3

 --  (lineare Notation, weil ich sie schöner finde)

 --  a)


 --  { f :: Y -> A -> B, g :: B -> Y, x :: B } |- f g x :: A -> B

 --  Sei K der Kontext { f :: Y -> A -> B, g :: B -> Y, x :: B }

 --  Versuchen wir zu verstehen was hier passiert:

 --   - der Typ 'A -> B' sagt uns fehlt noch ein Argument vom Typ 'A'
 --     => das heißt die Funktion 'f' hat bereits das Argument vom Typ 'Y'
 --        und es fehlt nur noch der Typ 'A'!

 --   - woher kriegt 'f' das Argument vom Typ 'Y'?
 --     => 'x' hat den Typ B, 'g' hat den Typ B -> Y
 --     => (g x) :: Y

 --  implizite Linksklammerung für Funktionsandwendung nicht vergessen!

 --  1. K |- (f g) x :: A -> B     (App 2, 3)
 --  2. K |- f g :: B -> A -> B    (App 4, 5)
 --  3. K |- x :: B                (Var)
 --  4. K |- f :: (B -> Y) -> B -> A -> B   (! error ! => 'f' hat im Kontext einen anderen Typ!)

 --  Fehler!

 --  Aus der Erklärung haben wir gesehen, wenn 'f (g x)' so geklammert wäre, würde die
 --  Typherleitung funktionieren!



 --  b)

 --  1. { f :: A -> Bool , z :: B } |- \y -> \x -> if f x then z else y z :: (B -> B) -> A -> B      (Abs 2)
 --  2. { f :: A -> Bool , z :: B, y :: B -> B } |- \x -> if f x then z else y z :: A -> B           (Abs 3)
 --  3. { f :: A -> Bool , z :: B, y :: B -> B, x :: A } |- if f x then z else y z :: B              (Cond 4, 5 ,6)

 --  Sei K der Kontext

 --  4. K |- f x :: Bool       (App 7, 8)
 --  5. K |- z   :: B          (Var)
 --  6. K |- y z :: B          (App 9, 10)
 --  7. K |- f   :: A -> Bool  (Var)
 --  8. K |- x   :: A          (Var)
 --  9. K |- y   :: B -> B     (Var)
 -- 10. K |- z   :: B          (Var)


 --  c)

 --  { } |- \x -> \f -> f x x :: A -> (A -> A) -> A

 --  Implizite Klammerung nicht vergessen!
--                          |
--                         / \
 -- 1. { } |- \x -> \f -> (f x) x :: A -> (A -> A) -> A   (Abs 2)
 -- 2. { x :: A } |- \f -> (f x) x :: (A -> A) -> A       (Abs 3)
 -- 3. { x :: A, f :: A -> A } |- (f x) x :: A            (App 4, 5)

 -- Sei K der Kontext

 -- 4. K |- f x :: A -> A                                 (App 6, 7)
 -- 5. K |- x   :: A                                      (Var)
 -- 6. K |- f   :: A -> A -> A                            (! error ! => 'f' hat im Kontext einen anderen Typ!)

 -- Wir wenden 'f' auf zwei Argumente angewendet, dabei nimmt es nur ein Argument => Fehler!