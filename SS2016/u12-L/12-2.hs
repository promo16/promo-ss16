-- | Aufgabe 12-2 - Fehlerbehandlung mit Exceptions
--

import Control.Monad     (unless)
import Control.Exception (try, SomeException())

-- | a) implementieren sie eine Funktion 'readUserFile :: IO String'
--
--   try :: Exception e => IO a -> IO (Either e a)
--
--   Diese Funktion wird in Haskell dafür benutzt um Exceptions zu fangen die
--   IO Funtionen schmeißen. Wir müssen den Typ davon angeben, weil er ihn nicht selber
--   inferieren kann
--
--   Da 'readFile' einen ungültigen Pfad bekommen kann, können wir das mit 'try' abfangen
--
--   Ist aber an sich extrem schlechter Stil - man sollte besser davor überprüfen ob der File existiert
--   (z.B aus System.Directory.doesFileExist :: FilePath -> IO Bool)

readUserFile :: IO String
readUserFile = do
    putStrLn "Validen Pfad zu einem Textfile angeben"
    path <- getLine
    eitherContent <- try (readFile path) :: IO (Either SomeException String)
    case eitherContent of
        Left err      -> print err >> readUserFile
        Right content -> return content


-- | b) schreiben sie eine Funktion 'readMaybe :: String -> Maybe Int' mithilfe von 'reads'

--   Sieht komisch aus, ist einfach eine Funktion die man kennen (googlen) muss.
--  
--  'reads' versucht den String soweit zu parsen wie es geht, gibt das Ergebnis als Liste von Tupeln raus,
--   wo das erste Element vom ersten Tupel der hoffentlich geparste Wert ist und der zweite Teil vom Tupel
--   der "Rest" vom String ist

readMaybe :: String -> Maybe Int
readMaybe str =
    case reads str of
        [(x, [])] -> Just x
        _         -> Nothing

-- | c) schreiben sie eine Funktion 'returnIndex :: [String] -> IO ()'
--
-- Wir wenden gleichzeitig readMaybe auf das Ergebnis von getLine, damit wir nicht aus Versehen
-- später etwas anderes mit dem Input von der Zeile machen
--
returnIndex :: [String] -> IO ()
returnIndex content = do
    putStrLn "Index der Liste eingeben"
    mint <- readMaybe <$> getLine
    case mint of 
        Just index -> putStrLn (content !! index)
        Nothing    -> putStrLn "Zahl nicht erkannt" >> returnIndex content


-- | Ausführlich

returnIndex' :: [String] -> IO ()
returnIndex' content = do
    putStrLn "Index der Liste eingeben"
    input <- getLine
    case readMaybe input of 
        Just index -> putStrLn (content !! index)
        Nothing    -> putStrLn "Zahl nicht erkannt" >> returnIndex' content


-- | d) schreiben sie eine Funktion 'main :: IO ()' die die oberen Funktion verknüpft

main :: IO ()
main = readUserFile >>= returnIndex . lines

-- | Ausführlich gehts natürlich auch
--
-- main = do
--     content <- readUserFile
--     returnIndex $ lines content


-- | e) returnIndex soll überprüfen ob der Index existiert
--
--   Wir können bei case-of Ausdrücken darunter einfach unsere bekannten Guards mit Einrückung benutzen
--
returnSafeIndex :: [String] -> IO ()
returnSafeIndex content = do
    putStrLn "Index der Liste eingeben"
    mint <- readMaybe <$> getLine
    case mint of 
        Just index
            | index >= 0 && index < length content -> putStrLn (content !! index)
            | otherwise                            -> putStrLn "Index out of bounds" >> returnSafeIndex content
        Nothing -> putStrLn "Zahl nicht erkannt" >> returnSafeIndex content

-- kann man auch mit 'unless :: Applicative f => Bool -> f () -> f ()' machen
--  (das Gegenteil von 'when :: Applicative f => Bool -> f () -> f ()')

returnSafeIndexM :: [String] -> IO ()
returnSafeIndexM content = do
    putStrLn "Index der Liste eingeben"
    mint <- readMaybe <$> getLine
    case mint of 
        Just index -> do
            unless (index >= 0 && index < length content) $ do
                putStrLn "Index out of bounds"
                returnSafeIndexM content
            putStrLn (content !! index)
        Nothing    -> putStrLn "Zahl nicht erkannt" >> returnSafeIndexM content

-- wem case-of-guards und monadische Aktionen noch schwer fallen ist hier eine andere Möglichkeit
-- diese Funktion zu definieren (nicht unbedingt hübsch, wenn man kein (>>) benutzt)

returnSafeIndexE :: [String] -> IO ()
returnSafeIndexE content = do
        putStrLn "Index der Liste eingeben"
        line <- getLine
        foo (readMaybe line)

    where

        foo :: Maybe Int -> IO ()
        foo (Just index)
            | index >= 0 && index < length content = putStrLn (content !! index)
            | otherwise = do
                putStrLn "Index out of bounds"
                returnSafeIndexE content
        foo Nothing = do
                putStrLn "Zahl nicht erkannt"
                returnSafeIndexE content