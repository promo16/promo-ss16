-- Aufgabe 6-1
--
-- Verzögerte Auswertung
--
--
-- Diese Art von Auswertung wird auch Lazy-Evaluation, oder Call-by-Need genannt.
-- Sie verbindet die Vorteile von applikativen / normalen Auswertungsreihenfolgen
-- und erlaubt uns einfachere Memoisation
--
-- Das bedeutet sprichwörtlich - wir rechnen nur das aus was wir gerade brauchen und 
-- wenn wir es mehrmals brauchen, speichert man es sich ab, damit man es nicht mehrmals rechnen muss.
--
-- Dieses Speichern wird durch sog. 'thunks' umgesetzt. Diese Ausdrücke führen wir mit 'let's ein.
--
-- Wir gehen zum Anfang genauso vor wie bei normaler Auswertungsreihenfolge.
--

{-

  Beispiel:

  Die Idee hinter dieser Aufgabe war zu zeigen, dass Ausdrücke nicht doppelt berechnet
  werden, wenn sie mehrmals angefordert werden. z.B:

  > (\x -> x + x) (1+2)

  Hier merkt sich GHC dass wir das 'x' zweimal brauchen und erstellt eine Referenz auf
  das Argument um es nur einmal zu berechnen. Da wir dies per Hand machen, benutzen wir
  dafür 'let' Ausdruck und benennen ihn mit einem unbenutzen Namen (hier 'a')

     (\x -> x + x) (1+2)
  => let a = (1+2)
     in (\x -> x + x) a

  Hier greift nun die 'let'-Regel aus der Vorlesung (4/F.10)

  => (\x -> x + x) a         [(a, (1+2))]

  Nun müssen wir die Referenz von a benutzen. Das signalisieren wir durch ^a

  => ^a + ^a                 [(a, (1+2))]

  Um weiter auszuwerten, müssen wir a ausrechnen. Das machen wir durch eine
  Nebenrechnung die wir eingerückt durchführen:

        => a                 [(a, (1+2))]
        => (1+2)             [(a, (1+2))]
        => 3                 [(a, (1+2))]

  Jetzt haben wir den Wert von 'a' und setzen ihn für alle ^a ein.
  Ich aktualisiere ihn auch in der Umgebung, aber dies war nicht in der Vorlesung spezifiziert.

  => 3 + 3                   [(a, 3)]

  => 6                       [(a, 3)]

  Let Regel Nr.2 - wenn der Ausdruck ausgewertet wurde, muss die Umgebung bereinigt werden

  => _                       []


  Die Hausaufgabe benutzt nur diese Regeln, ist aber leider etwas zu groß geraten.
  Uns wurde gesagt, dass wenn eine Aufgabe dazu in der Klausur kommt, dass sie sich an
  das Vorlesungsbeispiel orientieren wird (4./F.25).

  Alles was nicht in der Vorlesung spezifiziert war, was ich gemacht habe, kann wahrscheinlich
  weggelassen werden, aber ich würde immer eine Begründung hinschreiben.
   E.g "Umgebung bleibt in Nebenschritten gleich, wird weggelassen
   oder "Refrenzen werden nicht in der Umgebung aktualisiert, da wir sofort in alle Referenzen einsetzen
         und dieser Ausdruck nicht wieder benutzt wird"

-}

-- gegeben:
--
quadrat :: Num a => a -> a
quadrat = \x -> x*x

summe_quadrate :: Num a => a -> a -> a
summe_quadrate = \x y -> quadrat x + quadrat y

-- gefragt:
--
-- summe_quadrate (5-2) (quadrat (3-1))

{-

  => summe_quadrat (5-2) (quadrat (3-1))                    []            -- | Suchen von unbenutzer Variable für 5-2 und in ein let umwandeln

  => let a = 5 - 2 
     in summe_quadrat a (quadrat (3-1))                     []            -- | Analog für 'quadrat (3-1)' und Umgebung aktualisieren

  => let b = quadrat (3-1)
     in  summe_quadrat a b                                  [(a, 5 - 2)]  -- | summe_quadrat in Lambda-Form bringen und Umgebung aktualisieren

  => (\x y -> quadrat x + quadrat y) a b                    [(b, quadrat (3-1)), (a, 5 - 2)]  -- | Zeiger einsetzen (^a ist Zeiger auf a und ^b auf b)

  => quadrat ^a + quadrat ^b                                [(b, quadrat (3-1)), (a, 5 - 2)]  -- | quadrate in Lambda-Form bringen (parallel)

  => (\x -> x * x) ^a + (\x -> x * x) ^b                    [(b, quadrat (3-1)), (a, 5 - 2)]  -- | Einsetzen der Referenzen

  => ^a * ^a  + ^b * ^b                                     [(b, quadrat (3-1)), (a, 5 - 2)]  -- | Ausrechnen von links nach rechts - also ^a

      => a                                                  [(b, quadrat (3-1)), (a, 5 - 2)]
      => 5 - 2                                              [(b, quadrat (3-1)), (a, 5 - 2)]
      => 3                                                  [(b, quadrat (3-1)), (a, 5 - 2)]

  -- | Aktualisieren in der Umgebung und einsetzen

  => 3 * 3 + ^b * ^b                                        [(b, quadrat (3-1)), (a, 3)]      -- | Weiter ausrechnen - ^b

      => b                                                  [(b, quadrat (3-1)), (a, 3)]
      => quadrat (3-1)                                      [(b, quadrat (3-1)), (a, 3)]

-- | Wieder unbenutze Variable suchen für 3-1 und in ein let umwandeln
--   Hier haben wir die Annahme getroffen, dass wir die Umgebung immer aktualisiert wird
     Also überschreiben wir (b, quadrat (3-1)) mit(b, quadrat ^c)

      => let c = 3 - 1
      => in quadrat c                                       [(c, 3 - 1), (b, quadrat (3-1)), (a, 3)]

      => quadrat c                                          [(c, 3 - 1), (b, quadrat ^c), (a, 3)]

      => (\x -> x * x) c                                    [(c, 3 - 1), (b, quadrat ^c), (a, 3)]

      => ^c * ^c                                            -- ^c ist ein Zeiger auf c

          => c                                              [(c, 3 - 1), (b, quadrat ^c), (a, 3)]
          => 3-1                                            [(c, 3 - 1), (b, quadrat ^c), (a, 3)]
          => 2                                              [(c, 3 - 1), (b, quadrat ^c), (a, 3)]

      => 2 * 2                                              [(c, 2), (b, quadrat c^), (a, 3)]

      => 4                                                  [(c, 2), (b, quadrat c^), (a, 3)]

      => _                                                  [(b, quadrat 2), (a, 3)]

-- | b aktualisieren und das Ergebnis einsetzen

  => 3 * 3 + 4 * 4                                          [(b, 4), (a, 3)]

-- Ab hier nur noch Ausrechnen

  => 9 + 16                                                 [(b, 4), (a, 3)]

  => 25                                                     [(b, 4), (a, 3)]

  => _                                                      []

-}