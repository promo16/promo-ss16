-- Aufgabe 11 - 1: Monaden
--

import Data.Char (toUpper)


-- Die folgende Erklärung basiert auf dem Verständnis von dem Blatt 10
--
-- Für eine andere Erklärung:
--   http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--   http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

-- | Was sind Monaden?
--
--  Monaden sind grundsätzlich wieder nichts anderes als eine Typklasse in Haskell die (wir beschränken uns auf zwei) Funktionen beschreibt
--  
--  Es ist eine neue Abstraktion (ähnlich wie Functor und Applicative), die uns erlaubt Funktionen auf eine gewohnte
--  Art und Weise zu schreiben (do - Notation) und dennoch funktional zu bleiben (referenziell transparent was bedeutet gleicher Input => gleicher Output)
--
--  Um die do-Schreibweise zu benutzen, müssen wir aber zuerst die Bausteine dafür definieren, die die folgende Typklasse vorgibt:

{-

class Applicative m => Monad m where

    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

-}

-- Was machen diese Funktionen und wofür sind sie gut?
--
-- *) return :: m a
--
--   **) Diese Funktion ist äquivalent zu 'pure' aus der Applicative Klasse
--   **) Sie nimmt einen Wert und verpackt ihn in den Container (wird auch 'liften' genannt)

--   **) Beispiele:
--
--       ***) return 4 :: Maybe Int => (Just 4)
--       ***) return 4 :: [Int]     => [4]


-- *) (>>=) :: m a -> (a -> m b) -> m b
--
--   **) Diese Funktion nimmt zwei Argumente
--
--      ***) ein verpackten Wert (vom Typ 'm a') - in Z.54 'x'
--      ***) eine Funktion die ein 'a' nimmt und ein 'm b' zurückgibt - in Z.54 'f'
--
--   **) und liefert mir schlussendlich ein 'm b'
--
--   **) x >>= \y -> f y
--
--      ***) Wir entpacken 'x' und kriegen 'y' vom Typ 'a' raus
--      ***) Wir wenden 'f' auf 'y' an was uns automatisch den Typ 'm b' zurückgibt
--      ***) Das Lambda können wir uns wegen automatischem Einfügen von Argumenten sparen, deshalb sieht die Funktion
--           dann so aus:   x >>= f
--
--   **) Beispiele:

--            Maybe Int -> (Int -> Maybe Int)
--      ***) (Just 4) >>= (\x -> Just (x + 1))   => Just 5
--
--            Maybe Int -> (Int -> Maybe Int)
--      ***) Nothing  >>= (\x -> Just (x + 1))   => Nothing

--           [Int]    ->  (Int -> [Int])
--      ***) [4]      >>= (\x -> [x + 1])        => [5]
--      ***) [4,5]    >>= (\x -> [x + 1])        => [[5], [6]]

-- Diese müssen wieder die gleichen Regeln befolgen wie (<*>) aus Applicative: (für andere schöne Beispiele - https://wiki.haskell.org/Monad_laws)
--
-- Es muss gelten:
--    1) return a >>= f a       == f a      (Linksneutral)
--       *) Das bedeutet, wenn ich ein Wert in einen Container lifte und wieder raushole muss das gleiche rauskommen wie am Anfang
--       *) Die Funktionsapplikation kann man für das Verständnis mit 'return . id' wegabstrahieren:

--       return a >>= (return . id) == return . id a
--       return a >>= (return . id) == return a
--          |      |_            \______
--         /         \                  \
--    reinliften   rausholen      Identitätsfunktion verändert den Wert nicht

--       *) Beispiel:
--            **) (return 4 :: Maybe Int) >>= (return . id)
--            =>   Just 4                 >>= (return . id)
--            =>   Just 4                 >>= \x -> (return . id) x   (Wir entpacken Just 4 - es kommt 4 raus)
--            =>                                    (return . id) 4   (Identität verändert den Wert nicht)
--            =>                                     return       4
--            =>                                             Just 4   

--                (return . id) 4
--            =>  return        4
--            =>           Just 4

--    2) m  >>= return  == m        (Rechtsneutral)
--       *) Wenn wir einen Wert rausholen und dann wieder einpacken, muss der gleiche Wert rauskommen
--       *) Beispiel:

--           **) (Just 4) >>= return == (Just 4)
--               (Just 4) >>= \x -> return x       (Wir entpacken Just 4 - es kommt 4 raus)
--                                  return 4       (Wir verpacken 4 und wissen dass wir in der Maybe-Monade sind - es kommt 'Just 4' raus)
--                                    Just 4

--    3) (m >>= f) >>= g    ==     m >>= (\x -> f x >>= g)  (Assoziativität)
-- 
--       *) Man kann hier leider nicht folgendes hinschreiben,
--
--               (m >>= f) >>= g == m >>= (f >>= g)
--
--          was vielleicht die erste Idee von Assoziativität wäre, aber leider typecheckt (f >>= g) nicht.
--          'f' erwartet ein Argument, kriegt aber keins. Deswegen umgehen wir dieses Problem
--          indem wir mit dem Lambda ausführlich sagen, dass es später eins kriegt. Dies ist aber nur eine
--          Feinheit, die Idee ist dennoch die gleiche

--       *) Beispiel (eventuell hilfreich einzukommentieren, da Syntaxhighlightling das Verstehen erleichtert)

--          let f = \y -> Just (y + 1)  :: Num a        => a -> Maybe a
--          let g = \z -> Just (z / 2)  :: Fractional a => a -> Maybe a

--          ((Just 4) >>= f                 ) >>= g                     -- ('f' einsetzen)
--       => ((Just 4) >>= \y -> Just (y + 1)) >>= g                     -- (Wir entpacken 'Just 4', es kommt '4' raus)
--       =>                     Just (4 + 1)) >>= g 
--       =>                     Just 5        >>= g                     -- ('g' einsetzen)
--       =>                     Just 5        >>= \z -> Just (z / 2)    -- (Wir entpacken 'Just 5', es kommt '5' raus)
--       =>                                             Just (5 / 2)
--       =>                                             Just 2.5


--          (Just 4) >>= (\x -> f x >>= g)                                           -- ('f' einsetzen)
--          (Just 4) >>= (\x -> (\y -> Just (y + 1)) x >>= g)                        -- ('g' einsetzen)
--          (Just 4) >>= (\x -> (\y -> Just (y + 1)) x >>= (\z -> Just (z / 2)))     -- (Beta Reduktion - wir setzen für 'y' 'x' ein)
--          (Just 4) >>= (\y -> Just (y + 1) >>= (\z -> Just (z / 2)))               -- (wir entpacken 'Just 4' zu 4)
--                              Just (4 + 1) >>= (\z -> Just (z / 2))
--                              Just 5       >>= (\z -> Just (z / 2))                -- (wir entpacken 'Just 5' zu 5)
--                                                      Just (5 / 2)
--                                                      Just 2.5


-- Wem vielleicht schon aufgefallen ist - hier gibt es eine Einschränkung was alles eine Monade sein darf.
-- Oben bei der Instanzdeklaration steht, dass die Monade bereits ein Applicative sein muss.
-- Es gibt im Allgemeinen zwei Möglichkeiten diese Abstraktion zu sehen:

-- 1) Man definiert die Monadeninstanz, basierend darauf dann Applicative und dann Functor
-- 2) Man definiert zuerst Functor, dann Applicative und dann die Monad Instanz

-- In der Aufgabe 11-2 seht ihr beide Möglichkeiten - das sollte aber lediglich als Motivation verstanden werden,
-- dass diese Abstraktionen zusammenhängen. Solange ihr eine der Varianten nachvollziehen könnt, ist alles ok!

-- Wer die 11-2 ohne Hilfe reproduzieren kann, sollte keine Probleme in der Klausur zu ähnlichen Aufgaben haben.

-- Kommen wir nun aber zu der Hausaufgabe - hier sollte man mithilfe der Maybe Monade Funktionen erstellen,
-- die ein Bankkonto verwalten können.

-- a) definieren sie ein Typsynonym 'Money' - Int, 'Balance' - (Money, Money)

type Money   = Int
type Balance = (Money, Money)

-- b) definieren sie zwei Funktionen withdraw :: Money -> Balance -> Maybe Balance, deposit :: Money -> Balance -> Maybe Balance

-- Etwas komisch gedacht - wir haben 'debit' (Schulden) als permanente Schulden (negative Zahlen)
-- und 'credit' (Guthaben) (positive Zahlen) in einem Tupel gespeichert und wenn wir etwas abheben wollen,
-- müssen wir es als z.B withdraw -10 ... angeben, wenn wir 10 Euro abheben wollen

-- Diese Logik ist lediglich als Beispiel für eine Funktion mit Maybe gedacht (nicht unbedingt sinnvoll)

withdraw :: Money -> Balance -> Maybe Balance
withdraw money (debit, credit)
    | money <= 0  && (debit + money) + credit >= 0 = Just (debit + money, credit)
    | otherwise                                    = Nothing

deposit :: Money -> Balance -> Maybe Balance
deposit money (debit, credit)
    | money >= 0 = Just (debit, credit + money)
    | otherwise  = Nothing


-- c) banktag simulieren

-- Tag 1

--   99   (Einzahlung in ein leeres Konto)
--   -2   (Coffe-to-go)
--  -15   (Mittagessen)
--   19   (Einzahlen)
--  -80   (Abendessen)

-- Tag 2

--   99  (Einzahlung in ein leeres Konto)
--   -2  (Coffe-to-go)
-- -150  (Mittagessen beim Dallmayr)
--  -19  (Einzahlen)
--  -80  (Abendessen)

day1 :: Maybe Balance
day1 = do
    let s0 = (0,0)
    s1 <- deposit    99  s0
    s2 <- withdraw  (-2) s1
    s3 <- withdraw (-15) s2
    s4 <- deposit    19  s3
    s5 <- withdraw (-80) s4
    return s5

-- λ> day1
-- Just (-97,118)
-- it :: Maybe Balance

day2 :: Maybe Balance
day2 = do
    let s0 = (0,0)
    s1 <- deposit     99  s0
    s2 <- withdraw   (-2) s1
    s3 <- withdraw (-150) s2
    s4 <- deposit     19  s3
    s5 <- withdraw  (-80) s4
    return s5

-- λ> day2
-- Nothing
-- it :: Maybe Balance


-- d) Nutzen sie (>>=) statt der do-Notation

day1' :: Maybe Balance
day1' = let s0 = (0,0) in 
    deposit    99  s0 >>= \s1 ->
    withdraw  (-2) s1 >>= \s2 ->
    withdraw (-15) s2 >>= \s3 ->
    deposit    19  s3 >>= \s4 ->
    withdraw (-80) s4 >>= \s5 ->
    return s5

day2' :: Maybe Balance
day2' = let s0 = (0,0) in
    deposit     99  s0 >>= \s1 ->
    withdraw   (-2) s1 >>= \s2 ->
    withdraw (-150) s2 >>= \s3 ->
    deposit     19  s3 >>= \s4 ->
    withdraw  (-80) s4               -- hier brauchen wir eigentlich kein (>>=) und 'return',
                                     -- weil 'withdraw' uns bereits einen Maybe Balance Wert liefert

-- Es geht aber noch kürzer - das automatische Einfügen von Argumenten, wenn sie in der richtigen Reihenfolge kommen
-- erlaubt uns s[0-5] nicht mehr zu schreiben (wer es genauer wissen will - das heißt point free style, manchmal auch pointless...)

-- https://wiki.haskell.org/Pointfree

day1'' :: Maybe Balance
day1'' = deposit 99 (0,0) >>= withdraw (-2) >>= withdraw (-15) >>= deposit 19 >>= withdraw (-80)


-- e) Schreiben sie eine Funktion 'accountState :: Balance -> Maybe Account' die den Kontostand berechnet

type Account = Money

accountState :: Balance -> Maybe Account
accountState (debit, credit)
    | debit + credit >= 0 = Just $ debit + credit
    | otherwise           = Nothing

-- Zum Abschluss:

-- *) Man kann ganz gut an diesen Beispielen erkennen, dass Monaden für einen Zustandsinformation
--    automatisch mitschleifen können und uns die Schreibarbeit erleichtern

-- *) Neben dem (>>=) gibt es noch dessen automatisch abgeleitet Funktion (>>) die das Ergebnis der vorherigen
--    Berechnung verwirft
--
--    λ> deposit 99 (0,0) >> withdraw (-2) (0,0)
--    => Nothing, weil wir nicht genug Geld auf dem Konto hatten um 2 Geldeinheiten abzuheben
--
--
-- *) Es wurde zwar die Funktion 'fail' angesprochen, aber diese ist deprecated und sollte vergessen / nicht benutzt werden,
--    da viele Monade keine "gute" Möglichkeit haben einen Fehler zu schmeißen. Deshalb sollte man eine Fehlerbehandlung
--    auf eine andere Art und Weise verwirklichen

-- *) Wenn man die beiden Definitionen 'day1' und 'day1`' anschaut, dann sieht man eine große Ähnlichkeit - jeglicher Code
--    den ihr bisher in do-Notation geschrieben habt, wird von Haskell in die (>>=)-Notation umgeschrieben. Das nennt sich
--    "syntactic sugar", damit wir in gewohnten imperativen Stil Prgramme schreiben können, sie aber in Wirklichkeit
--    echt funktional sind

-- Übungen:

-- Schreibe den folgenden Code in die 'do-Notation' um und gib den Typ, sowie das Ergebnis an (falls möglich/nötig)
-- Wichtig - es ist der Typ der Funktion wichtig, nicht der ausgewertete Ausdruck (GHCi wertet automatisch aus)

u01 = return 4 >>= \x -> Just x

u02 = pure 4   >>= \x -> Just x

u03 = Nothing  >>= \x -> Just x >>= \y -> Just x

u04 = Nothing  >>= Just         >>= \_ -> Just 1

u05 :: Maybe Int
u05 = pure (\x -> x + 1) <*> pure 5

u06 :: Maybe (Maybe a)
u06 = pure id <*> pure Nothing

u07 = print 5 >>= \_ -> putStrLn "Hello"

u08 = getLine >>= \str -> putStrLn ("Hello" ++ str)              -- Beispieleingabe: "Bruce"

u09 = print (1,2) >>= \x -> print x

u10 = getLine >>= \str -> (return . filter (/= 'a')) str         -- Beispieleingabe: "abccda"

u11 = ((+1) >>= (*)) 2    -- Tipp: (-> r) Instanz

-- Schreibe den folgenden Code in die 'bind-Notation' um und den gib den Typ, sowie das Ergebnis an (falls möglich/nötig)


u12 :: Maybe Int
u12 = do
    f <- pure (+)
    x <- return 1
    y <- pure 10
    return (f x y)

u13 :: [Int]
u13 = do
    f <- pure (+)
    x <- return 1
    y <- pure 10
    return (f x y)

u14 = do
    str1 <- getLine                         -- Beispieleingabe: "abc"
    str2 <- getLine                         -- Beispieleingabe: "def"
    return (str1 ++ str2)

u15 = do
    str <- fmap (drop 5) getLine            -- Beispieleingabe: "abcdefg"
    return str

u16 = do
    putStrLn "How are you?"
    getLine                                   -- Beispieleingabe: "shitty"
    feels <- getLine                          -- Beispieleingabe: "well"
    putStrLn $ (map toUpper feels) ++ "?!...seriously?!"

u17 = do
    x <- [1,2]
    return (x + x)

u18 = do
    x <- (*2) <$> [1,2]
    y <- [1,2]
    return (x + y)

u19 = do
    x <- (read :: String -> Double) <$> Just "5"
    y <- test x
    return (10 / y)
  where
    test 0 = Nothing
    test n = Just n

u20 = do
    x <- (read :: String -> Double) <$> Just "0"
    y <- test x
    return (10 / y)
  where
    test 0 = Nothing
    test n = Just n











-- Lösungen:

-- u01 = return 4 >>= \x -> Just x
u01' :: Num a => Maybe a
u01' = do
    x <- return 4
    Just x

-- => Just 4

-- u02 = pure 4   >>= \x -> Just x
u02' :: Num a => Maybe a
u02' = do
    x <- pure 4
    Just x

-- => Just 4

-- u03 = Nothing  >>= \x -> Just x >>= \y -> Just x
u03' :: Maybe a
u03' = do
    x <- Nothing
    y <- Just x
    Just x

-- => Nothing

-- Erklärung: Die Instanz von Maybe ist so definiert, dass wenn einer der Werte 'Nothing' zurückgibt, sie alle folgenden Berechnungen
--            verwirft und 'Nothing' zurückgibt

-- u04 = Nothing  >>= Just         >>= \_ -> Just 1
u04' :: Num a => Maybe a
u04' = do
    x <- Nothing
    _ <- Just x
    Just 1

-- => Nothing

-- Erklärung: Analog zu u03

-- u05 = pure (\x -> x + 1) <*> pure 5
u05' :: Maybe Int
u05' = do
    f <- pure (\x -> x + 1)
    x <- pure 5
    return (f x)

-- Erklärung: Wir verpacken die Funktion (\x -> x+1) mit einem Just (\x -> x+1) in Z.418 und entpacken ihn gleichzeitig wieder
--            Wir verpacken den Wert 5 mit einem Just 5 in Z.419 und entpacken ihn gleichzeitig wieder
--            Wir verpacken das Ergebnis von '(\x -> x + 1) 5' in ein Just

-- => Just 6

-- u06 = pure id <*> pure (Nothing)
u06' :: Maybe (Maybe a)
u06' = do
    f <- pure id
    x <- pure Nothing
    return (f x)

-- => Just Nothing

-- Erklärung: Wieder ein Nothing in der Funktion - warum kriegen wir Just Nothing raus?
--            Wir verpacken 'id' mit 'Just' und entpacken es wieder mit dem Namen f, also f := id
--            Wir verpacken 'Nothing' mit 'Just' und entpacken es wieder mit dem Namen x, also x := Nothing
--            Wir verpacken das Ergebnis von 'id Nothing' mit einem Just
--            Just (id Nothing) => Just Nothing


-- u07 = print 5 >>= \_ -> putStrLn "Hello"
u07' :: IO ()
u07' = do
    _ <- print 5     -- 'print 5' wäre hier auch ok
    putStrLn "Hello"

--    5       (lediglich Seiteneffekte)
--    Hello   (lediglich Seiteneffekte)
-- => ()      (Rückgabewert)

-- Erklärung: Wenn wir ein Ergebnis einer Funktion wegschmeißen, können wir uns einfach den '<-'-Pfeil sparen


-- u08 = getLine >>= \str -> putStrLn ("Hello " ++ str)         -- Beispieleingabe: "Bruce"
u08' :: IO ()
u08' = do
    str <- getLine
    putStrLn ("Hello " ++ str)

-- => "Hello Bruce"

-- u09 = print (1,2) >>= \x -> print x
u09' :: IO ()
u09' = do
    x <- print (1,2)
    print x

--    (1,2) (Seiteneffekte)
--    ()    (Seiteneffekte)
-- => ()    (Rückgabewert)

-- Erklärung: Der 'void'-Typ () hat einen einzelnen Wert - nämlich '()'. Den kann man sich natürlich auch ausgeben lassen.


-- u10 = getLine >>= \str -> (return . filter (/= 'a')) str         -- Beispieleingabe: "abccda"
u10' :: IO String
u10' = do
    str <- getLine
    (return . filter (/= 'a')) str

-- => "bccd"

-- Erklärung: Die Funktion 'filter' lässt alles durch was das Prädikat erfüllt. In diesem Fall alles, was kein 'a' war

-- u11 = ((+1) >>= (*)) 2    -- Tipp: (-> r) Instanz
u11' :: Num a => a
u11' = do
    (*) ((+1) 2) 2

-- => 6

-- Erklärung: Einfach diese Instanz abgeschrieben
--            http://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Base.html#line-641
--            Das Beispiel sollte einfach nur zeigen, dass unsere normale Funktionsapplikation auch eine Monad/Applicative/Funktor Instanz besitzt
--            Diese aber auswendig zu wissen ist extrem unnötig

-- Schreibe den folgenden Code in die 'bind-Notation' um und den gib den Typ, sowie das Ergebnis an (falls möglich/nötig)

-- u12 :: Maybe Int
-- u12 = do
--     f <- pure (+)
--     x <- return 1
--     y <- pure 10
--     return (f x y)

u12' :: Maybe Int
u12' = 
    pure (+) >>= \f ->
    return 1 >>= \x ->
    pure 10  >>= \y ->
    return (f x y)

-- => Just 11

-- Erklärung: Wie man hier wieder schön sieht - 'do-Notation' ist "syntactic sugar" für die 'bind'-Notation
--            Da pure = return, können wir mit beiden Funktionen den Maybe Typ erstellen


-- u13 :: [Int]
-- u13 = do
--     f <- pure (+)
--     x <- return 1
--     y <- pure 10
--     return (f x y)

u13' :: [Int]
u13' =
    pure (+) >>= \f ->
    return 1 >>= \x ->
    pure 10  >>= \y ->
    return (f x y)

-- => [11]

-- Erklärung: Da hier nirgendso spezielle Monaden benutzt werden, musste ich den Typ angeben. Davon ist natürlich das Resultat abhängig. (Siehe u12)

-- u14 = do
--     str1 <- getLine                         -- Beispieleingabe: "abc"
--     str2 <- getLine                         -- Beispieleingabe: "def"
--     return (str1 ++ str2)

u14' :: IO String
u14' =
    getLine >>= \str1 ->                       -- Beispieleingabe: "abc"
    getLine >>= \str2 ->                       -- Beispieleingabe: "def"
    return (str1 ++ str2)

-- => "abcdef"

-- Erklärung: Hier kann man gut erkennen, dass der Zustand der ersten Funktionen auch in den nachfolgenden sichtbar ist
--            (str1 kann nach mehreren >>= benutzt werden)
-- 

-- u15 = do
--     str <- fmap (drop 5) getLine            -- Beispieleingabe: "abcdefg"
--     return str                              

u15' :: IO String
u15' = fmap (drop 5) getLine                   -- Das 'return' kann man sich sparen, weil getLine automatisch den String in IO zurückgibt

u15'' = drop 5 <$> getLine                     -- Abkürzung für 'fmap', dafür können wir uns die Klammern sparen

-- => "fg"

-- Erklärung: Hier kann man gut erkennen wie deskriptiv Haskell sein kann - und das in 18 Zeichen.
--            Die Funktion (drop 5) wird auf das Ergebnis von 'getLine' angewendet obwohl diese in der IO ist.
--            Das erlaubt uns sehr einfach "pure" Funktionen mit Funktionen mit Seiteneffekten zu verbinden
--            'drop 5' schmeißt die ersten 5 Elemente einer Liste weg und gibt den Rest zurück

-- u16 = do
--     putStrLn "How are you?"
--     getLine                                   -- Beispieleingabe: "shitty"
--     feels <- getLine                          -- Beispieleingabe: "well"
--     putStrLn $ (map toUpper feels) ++ "?!...seriously?!"

u16' :: IO ()
u16' = 
     putStrLn "How are you?" >>
     getLine                 >>
     getLine                 >>= \feels ->
     putStrLn $ (map toUpper feels) ++ "?!...seriously?"

-- => "WELL?!...seriously?!"

-- Erklärung: Wenn man den Funktionsrückgabewert nicht haben will, kann man (>>) benutzen, das ihn automatisch wegschmeißt.
--            Da 'feels' ein String ist, kann ich darüber mappen.
--            'toUpper' wandelt alles in Großbuchstaben um

-- u17 = do
--     x <- [1,2]
--     return (x + x)

u17' :: Num a => [a]
u17' = [1,2] >>= \x -> return (x + x)  -- zur Erinnerung (>>=) für Listen ist folgendermaßen definiert
                                       --
                                       --           xs >>= f  = [y | x <- xs, y <- f x]
-- =>  [2,4]

-- Erklärung: In die List-Comprehention einsetzen und auswerten. Das 'return' fällt in diesem Fall weg, weil wir ja 
--            in 'y <- f x' wieder das Ergebnis entpacken.
-- 
--   [1,2] >>= \z -> return (z + z)                 -- x nach z umbenannt
--   [y | x <- [1,2], y <- (\z -> return (z+z)) x]  -- für x die '1' einsetzen
--                    y <- return 1+1
--                    y <- [1+1]
--                    y <- [2]
--                    2

--   [2] ++ [y | x <- [2], y <- (\z -> return (z+z)) x]
--                              y <- return 2+2    -- für x die '2' einsetzen
--                              y <- [2+2]
--                              y <- [4]
--                              4

--    [2] ++ [4] ++ [y | x <- [], y <- (\z -> return (z+z)) x]
--    [2] ++ [4] ++ []                              -- nichts mehr zum einsetzen
--    [2,4]


-- u18 = do
--     x <- (*2) <$> [1,2]
--     y <- [1,2]
--     return (x + y)

u18' :: Num a => [a]
u18' = (*2) <$> [1,2] >>= \x -> [1,2] >>= \y -> return (x + y)

--              [2,4] >>= \x -> [1,2] >>= \y -> return (x + y)                                   (*2 über die Liste mappen)
--              [z | x <- [2,4], z <- (\x -> [1,2] >>= \y -> return (x + y)) x]                  (bind in list-comprehention umformen)
--              [z | x <- [2,4], z <- (\x -> [b | a <- [1,2] b <- (\y -> return (x + y)) a]) x]  (inneres bind umformen)
--              [z | x <- [2,4], z <- (\x -> [x + 1, x + 2]) x]                                  (innere list-comprehention auswerten)
--                               z <- [2 + 1, 2 + 2]                                             (innere list-comprehention für 2 auswerten)
--                               z <- [3,4]

--   [3,4] ++   [z | x <- [2], z <- (\x -> [x + 1, x + 2]) x]                                    (innere list-comprehention für 4 auswerten)
--                               z <- [4 + 1, 4 + 2]
--                               z <- [5,6]
--   [3,4] ++ [5,6]
--   [3,4,5,6]

-- Hier auch am besten einkommentieren, damit Syntaxhighlighting helfen kann
-- Das sieht am Anfang etwas kompliziert aus, aber eigentlich ist es einfach nur eine Verknüpfung von jeder mit jedem. Die 'do-Notation'
-- zeigt das sehr schön auf. 

-- u18 = do
--     x <- (*2) <$> [1,2]      -- für jedes x in dieser Liste werden alle folgenden Zeilen ausgeführt
--     y <- [1,2]               -- für jedes y in dieser Liste werden alle folgenden Zeilen ausgeführt
--     return (x + y)           -- da wir nun addieren, addieren wir jedes x mit jedem y und fassen es in eine Liste zusammen
                                -- nun einfach jeder mit jedem -> (1*2) + 1, (1*2) + 2 und (2*2) + 1, (2*2) + 2
--                                                                  |     |    |     |       |     |    |     |
--                                                                x[0]   y[0] x[0]  y[1]   x[1]   y[0] x[1]  y[1]

-- u19 = do
--     x <- (read :: String -> Double) <$> Just "5"
--     y <- test x
--     return (10 / y)
--   where
--     test 0 = Nothing
--     test n = Just n

u19' :: Maybe Double
u19' = (read :: String -> Double) <$> Just "5" >>= test >>= return . (10.0 /)
  where
    test 0 = Nothing
    test n = Just n

-- oder ausführlich

u19'' :: Maybe Double
u19'' = (read :: String -> Double) <$> Just "5" >>= \x -> test x >>= \y -> return (10.0 / y)
  where
    test 0 = Nothing
    test n = Just n

-- => Just 2.0


-- Erklärung: Hier können wir wieder automatisches Einsetzen der Argumente benutzen (wer das aber nicht mag, muss es nicht lernen)
--            Ich wollte nur ein paar Beispiele drannehmen, weil das in der Vorlesung ansatzweise behandelt wurde und damit ihr
--            euch nicht irgendwann wundert was das bedeutet
-- 
--            read versucht einen String zu dem Datentyp zu parsen, den ihr ihm in der Typsignatur vorgebt
--
--            Indem wir eine Funktion 'test' haben die überprüft ob diese Zahl eine 0 ist, können wir ohne Schwierigkeit
--            darauf aufbauen und uns sicher sein, dass wenn das mal 0 sein sollte, dass die Berechnung sofort abbricht (wie in u20)

-- u20 = do
--     x <- (read :: String -> Double) <$> Just "0"
--     y <- test x
--     return (10 / y)
--   where
--     test 0 = Nothing
--     test n = Just n

u20' :: Maybe Double
u20' = (read :: String -> Double) <$> Just "0" >>= \x -> test x >>= \y -> return (10.0 / y)
  where
    test 0 = Nothing
    test n = Just n

-- => Nothing