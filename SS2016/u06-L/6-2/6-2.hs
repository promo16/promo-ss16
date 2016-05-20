import Prelude hiding (null)

-- Aufgabe 6-2
--
-- Verzögerte Auswertung (analog zur 6-1)
-- 
-- Beispiele und Erklärung stehen in 6-1.
--
--
-- gegeben:

quadrat :: Num a => a -> a
quadrat = \x -> x*x

summe_quadrate :: Num a => a -> a -> a
summe_quadrate = \x y -> quadrat x + quadrat y

null :: Num b => a -> b
null x = 0

f :: Num a => a -> a
f n = if null (quadrat n) /= n then summe_quadrate (n-2) (n-1) else n

-- gefragt:
-- 
-- 'f 3'   mit verzögerter Auswertung evaluieren

{-

-- | Es wurde in der Vorlesung nicht spezifiziert ob wir für Konstanten ein 'let' einführen,
--   aber damit es einheitlicher wird, habe ich es hier benutzt. Falls ihr es nicht benutzt,
--   einfach dazuschreiben warum: E.g "Konstante primitive Typen sind bereits so vereinfacht, dass man
--                                     sie ohne zusätzlichen Aufwand einsetzen kann"

--   Ich werde jeden Ausdruck der ausgewertet wird mit einem 'let' einführen, damit das Schema gleich bleibt. (Ist in der Vorlesung jedoch nicht spezifiziert)

     f 3                                                                             []       -- | Suchen von unbenutzer Variable für 3 als let einführen

  => let d = 3
     in f d                                                                          []       -- | d in die Umgebung aufnehmen und f in Lambda-Form bringen

  => (\n -> if null (quadrat n) /= n then summe_quadrate (n-2) (n-1) else n) d       [(d, 3)] -- | Nun greift die if-regel - zuerst Bedingung auswerten

  => let e = (quadrat ^d)
     in if null e /= ^d then summe_quadrate (^d-2) (^d-1) else ^d         [(d, 3)] -- | Hier wird die Annahme getroffen dass wir jeden Ausdruck zuerst in die Umgebung einführen

  => if (\x -> 0) e /= ^d then summe_quadrate (^d-2) (^d-1) else ^d       [(e, quadrat ^d), (d, 3)] -- | (\x -> 0) auswerten

  => if 0 /= ^d then summe_quadrate (^d - 2) (^d - 1) else ^d             [(e, quadrat ^d), (d, 3)] -- | a soweit einsetzen damit wir die Bedingung auswerten können

  => if True    then summe_quadrate (^d - 2) (^d - 1) else ^d             [(e, quadrat ^d), (d, 3)] -- | if auswerten und 'e' aus der Umgebung werfen, weil die if-Bedingung ausgewertet wurde

  => summe_quadrat (^d-2) (^d-1)                            [(d, 3)]     -- | Suchen von unbenutzer Variable für ^d-2 und in ein let umwandeln

  => let a = ^d - 2
     in summe_quadrat a (^d-1)                              [(d, 3)]      -- | Analog für 'quadrat (^d-1)' und Umgebung aktualisieren

  => let b = (^d-1)
     in  summe_quadrat a b                                  [(a, ^d - 2), (d, 3)]  -- | summe_quadrat in Lambda-Form bringen und Umgebung aktualisieren

  => (\x y -> quadrat x + quadrat y) a b                    [(b, ^d-1), (a, ^d-2), (d, 3)]  -- | Zeiger einsetzen (^a ist Zeiger auf a und ^b auf b)

  => quadrat ^a + quadrat ^b                                [(b, ^d-1), (a, ^d-2), (d, 3)]  -- | quadrate in Lambda-Form bringen (parallel)

  => (\x -> x * x) ^a + (\x -> x * x) ^b                    [(b, ^d-1), (a, ^d-2), (d, 3)]  -- | Einsetzen der Pointer

  => ^a * ^a  + ^b * ^b                                     [(b, ^d-1), (a, ^d-2), (d, 3)]  -- | Ausrechnen von links nach rechts - also ^a

      => a                                                  [(b, ^d-1), (a, ^d-2), (d, 3)]
      => ^d - 2                                             [(b, ^d-1), (a, ^d-2), (d, 3)]
      => 3 - 2                                              [(b, ^d-1), (a, ^d-2), (d, 3)]
      => 1                                                  [(b, ^d-1), (a, ^d-2), (d, 3)]

  -- | Aktualisieren in der Umgebung und einsetzen

  => 1 * 1 + ^b * ^b                                        [(b, ^d-1), (a, 1), (d, 3)]      -- | Weiter ausrechnen - ^b

      => b                                                  [(b, ^d-1), (a, 1), (d, 3)]
      => ^d-1                                               [(b, ^d-1), (a, 1), (d, 3)]
      => 3-1                                                [(b, ^d-1), (a, 1), (d, 3)]
      => 2                                                  [(b, ^d-1), (a, 1), (d, 3)]

-- | b aktualisieren und das Ergebnis einsetzen

  => 1 * 1 + 2 * 2                                          [(b, 2), (a, 1), (d, 3)]

-- Ab hier nur noch Ausrechnen

  => 1 + 4                                                 [(b, 2), (a, 1), (d, 3)]

  => 5                                                     [(b, 2), (a, 1), (d, 3)]

-- | Hier habe ich die inneren 'lets' gleichzeitig aus der Umgebung geschmissen

  => _                                                      [(d, 3)]

-- | Wurde in der Vorlesung nicht spezifiziert, ob man sie nacheinander rausschmeißen muss, wenn sie
--   auf verschiedenen 'Ebenen' eingeführt worden sind. Falls ihr das macht/nicht macht, einfach dazuschreiben warum:
--   E.g Da man mit der Auswertung fertig ist, beeinflusst die Reihenfolge des Löschens aus der Umgebung nicht das Ergebnis

  => _                                                      []

-}