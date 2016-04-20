----------------------------------------------------------------------------
-- Vorlage für Übung H11-1 zur Vorlesung "Programmierung und Modellierung"
-- am 7.07.2015 an der LMU München
-- verfasst von Simon Weinzierl & Steffen Jost
--

{-
  HINWEISE:

  Sie benötigten das Paket monad-par,
  welches Sie mit

  > cabal install monad-par

  installieren müssen.
  Das Tool cabal-install sollte bereits mit der
  Haskell-Plattform auf Ihren Rechner installiert worden sein.

  Falls Sie Probleme mit der lokalen Installation haben,
  dann verwenden Sie einfach die Rechner im CIP-Pool.
  Dies ist auch aus der Ferne möglich.

  Informationen zum Remote-Login am CIP-Pool finden Sie auf:
    http://www.rz.ifi.lmu.de/FAQ/index.html
  im Abschnitt ``Von zu Hause/remote aus\dots''


  DIE AUFGABE:

  Gegeben ist:
    - (e,modulusN) der öffentliche RSA-Schlüssel
    - eine verschlüsselte Nachricht

  Beschleunigen Sie die Berechenung von slowFactor mit Hilfe der par-Monade!

  Sie müssen lediglich den Dummy "parallelFactorisation"
  neu programmieren (und ggf. Hilfsfunktionen),
  sonst muss nichts am Code verändert werdn.

  Um das Programm mit Verwendung von bis zu 4 Kernen laufen zu lassen,
  muss wie folgt kompiliert werden:

  ghc H11-1.hs -O2 -threaded -with-rtsopts="-N4"

  Um das Programm mit Verwendung von bis zu 8 Kernen laufen zu lassen,
  muss wie folgt kompiliert werden:

  ghc H11-1.hs -O2 -threaded -with-rtsopts="-N8"

  Natürlich muss ich Rechner über die entsprechende Anzahl Kerne verfügen.
  Die Rechner am CIP-Pool haben in der Regel 4 Kernen.

  Mit 4 Kernen erzielten wir einen passablen Speedup-Faktor von ca. 3 bis 3,3.
-}

import Control.Monad.Par
import Control.Monad
import Data.Maybe (isJust)
import Data.List (sort)
import Data.Char (ord,chr)              -- für verwendete Hilfsfunktionen
import Data.Bits (shift,shiftR,testBit) -- für verwendete Hilfsfunktionen



main :: IO () -- Berechnung dauert einige Minuten!
main =
  let -- öffentlich bekannter Schlüssel:
      (e,modulusN) = (65537, 14319953997750485653)
      -- Abgefangene, kodierte Nachricht:
      cipherText   = [3599095440359684280,8429158900661875260,10178806454520659127,6551064472958297887,5651186809464672658,2111885777557764257,13798324382766252905,6527790781908254521,5242330391010611359,11382816024842545353,844653399386628596,3567901469909441125,2814364451569929674,1877941445914697932,10937769801699527900,7837560062010901347,7031004325217398127,12765689480296485439,9348427278748733404,11906767186059199700,8011393134059371414,5832442572366870044]
      -- kleinstmögliche Primzahl für die Suche
      minp = 3 -- !NICHT VERÄNDERN!
      -- größtmögliche   Primzahl für die Suche
      maxp = sqrtFloor modulusN -- !NICHT VERÄNDERN!
  {-
    *** WICHTIGER HINWEIS ***
    Natürlich könnte man die Berechnung auch beschleunigen,
    in dem man das Such-Intervall verkleinert. Das funktioniert
    auch mit einem festen Beispiel wie hier: wenn man die Lösung p schon kennt,
    sucht man eben nur im Einer-Intervall (p..p).
    Bei der Bewertung werden wir Ihre Abgabe auch mit anderen Schlüsseln testen,
    d.h. lassen Sie das Intervall unverändert!!!
  -}
  in case parallelFactorisation (minp,maxp) modulusN of
    Nothing  -> putStrLn "Kein Primfaktor gefunden. Entschlüsselung gescheitert!"
    (Just factor) -> do
      let [p,q] = sort $ [factor, modulusN `div` factor]
      putStrLn $ "Die Faktoren des gegebenen Modulus sind " ++ show p ++ " und " ++ show q ++ "."
      let d = getDecryptionExponent e p q
      putStrLn $ "Damit ist der Entschlüsselungsexponent " ++ (show d) ++ "."
      let clearText = decodeLargeText (d,modulusN) cipherText
      putStrLn ("Damit ist der zu entschlüsselnde Klartext:\n  \"" ++ clearText ++ "\".")

--------------------------------------------------
-- Faktorisierung

parallelFactorisation :: (Integer,Integer) -> Integer -> Maybe Integer
parallelFactorisation (from, to) p = runPar $ do

-- first we have to divide the range in four equal pieces!
  let delta = (to - from) `div` 4

  ivarResults <- forM [1..4] (\n -> spawnP $ slowFactor (from + (n - 1) * delta, from + n * delta) p)
  getFirst ivarResults

 where getFirst []     = return Nothing
       getFirst (x:xs) = do
         result <- get x
         case result of
           Nothing -> getFirst xs
           (_)     -> return result

-- Findet den ersten Faktor in einem gegebenen Bereich.
slowFactor :: (Integer,Integer) -> Integer -> Maybe Integer
slowFactor (lo,hi) n
    | lo > hi            = Nothing
    | lo > (sqrtFloor n) = Nothing
    | otherwise = slowFactorRec lo n
  where
    slowFactorRec :: Integer -> Integer -> Maybe Integer
    slowFactorRec curr m
      | mod m curr == 0 = Just curr
      | curr+1 <= hi    = slowFactorRec (curr+1) m
      | otherwise       = Nothing

--------------------------------------------------
-- Kryptographische Funktionen


-- Verschlüsselt mit angegebenem Verschlüsselungsexponenten.
encrypt :: (Integer, Integer) -> [Integer] -> [Integer]
encrypt (e,modulusN) = map (\x -> powModInteger x e modulusN)

-- Entschlüsselt mit berechnetem Entschlüsselungsexponenten.
decrypt :: (Integer, Integer) -> [Integer] -> [Integer]
decrypt (d,modulusN) = map (\x -> powModInteger x d modulusN)

decrypt1 :: (Integer, Integer) -> Integer -> Integer
decrypt1 (d,modulusN) x = powModInteger x d modulusN


getDecryptionExponent :: Integer -> Integer -> Integer -> Integer
getDecryptionExponent e p q = fst $ erwEuklid e $ (p-1)*(q-1)


-- Hier wird angenommen, dass die Characters nur Bytelänge haben...
convertToString :: Integer -> String
convertToString 0 = []
convertToString n
    | n < 0 = error "No negative Integers are allowed!"
    | otherwise = let c = mod n 256
                  in (chr (fromInteger c)) : (convertToString (div (n-c) 256))

-- Hier auch...
convertToInteger :: String -> Integer
convertToInteger = fst . ( foldl (\(g,s) c -> (g + (shift (toInteger $ ord c) (s*8)), s+1) ) (0,0) )

-- Soll verwendet werden, um längere Texte zu kodieren, um sie später zu verschlüsseln.
encodeLargeText :: String -> [Integer] -- eigentlich reicht Int64
encodeLargeText = (map convertToInteger) . (splitItUp 7)

-- Soll verwendet werden, um längere Texte zu dekodieren.
decodeLargeText :: (Integer,Integer) -> [Integer] -> String
decodeLargeText privateKey code =
  -- concatMap (convertToString . (decrypt1 privateKey)) code
  concat $ map convertToString $ decrypt privateKey code

----------------------------------------------------------------
-- Mathematische Hilfsfunktionen:

{- Hinweis:
  Zur Vereinfachung der Aufgabe liefern wir alle
  benötigten mathematischen Hilfsfunktionen hier mit.
  Die angegebenen Definition sind nicht unbedingt die effizientesten.

  Hier wäre es besser, auf effiziente Bibliotheken zurückzugreifen:
    GHC.Integer.GMP.Internals -- The ​GNU Multiple Precision Arithmetic Library (GMP)
    Math.NumberTheory         -- arithmoi package

  Sämtlicher folgender Code funktioniert zwar für diese Aufgabe,
  von einer anderweitigen Verwendung ist aber abzusehen:
-}

sqrtFloor :: Integer -> Integer
sqrtFloor x = floor $ sqrt (fromInteger x) -- Feherhaft!
-- Diese Definiton ist problematisch aufgrund der mangelnden Präzision!
-- Für die Aufgabe reicht es, weil der modulus nur 64-Bit ist

-- Schnelles Potentzieren mit Modulo, besser  GHC.Integer.GMP.Internals.powModInteger verwenden, braucht jedoch UnboxedTuples Erweiterung
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger base expon modulus = pmAux base expon
  where
    pmAux _ 0     = 1
    pmAux b e
      | even e    =  pmAux (b*b `mod` modulus) (e `div` 2)
      | otherwise = (pmAux (b*b `mod` modulus) (e `div` 2)) * b `mod` modulus

-- Interessanterweise ist die endrekursive Version langsamer,
-- wenn man ghc mit Option "-O2" wildes optimieren erlaubt:
powModInteger1 :: Integer -> Integer -> Integer -> Integer
powModInteger1 base expon modulus = pmAux base expon 1
  where
    pmAux _ 0 acc = acc
    pmAux b e acc
      | even e    = pmAux (b*b `mod` modulus) (e `div` 2)     acc
      | otherwise = pmAux (b*b `mod` modulus) (e `div` 2)  $! acc * b `mod` modulus

-- erweiterter Euklidischer Algorithmus
erwEuklid :: Integer -> Integer -> (Integer, Integer)
erwEuklid m n
  | 0 == (n `mod` m) = (1,0)
  | otherwise        =
      let (x',y') = erwEuklid (n `mod` m) m
      in  -- trace (show $ ((m,n),(n `div` m),(n `mod` m),(x',y'))) -- trace zur Ausgabe der Zwischenwerte wie bei Papier-Rechnung
          (y'-x'*(n `div` m), x')


----------------------------------------------------------------
-- Allgemeine Hilfsfunktionen:

-- Zerschneidet eine Liste in gleich lange Sublisten.
splitItUp :: Int -> [a] -> [[a]]
splitItUp _ [] = []
splitItUp i l  = (take i l) : (splitItUp i (drop i l))
