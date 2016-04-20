module Main where



-- | H6-1

data Baum = Blatt Char | Knoten Baum Char Baum

-- analog zu
-- data Baum = Blatt { blabel :: Char } | Knoten { left :: Baum , klabel :: Char, right :: Baum }

dfCollect :: Baum -> String
dfCollect (Blatt c)               = [c]
dfCollect (Knoten links c rechts) = c : dfCollect links ++ dfCollect rechts

-- gegeben:

--  - dfCollect
--  - (++) terminiert immer
--  - (:)  terminiert immer
--  - keine Einschränkung für unsere Definitionsmenge -> Also alle möglichen Konstruktoren von Baum

-- z.z durch eine Abstiegsfunktion:

--    - dfCollect terminiert

-- AUF)

--   Führen die rekursiven Aufrufe aus der definierten Menge raus?

--   Wir bleiben in der definierten Menge da die rekursiven Aufrufe immer den Datentyp Baum
--   als Argument kriegen! (++) und (:) sind total definiert und damit passiert hier kein Unfug!

-- DEF)

--   Werden alle möglichen Argumente für unsere Menge abgefangen?

--   Ja, da wir beide Konstruktoren abfangen und im Pattern-Match nicht gegen ein bestimmtes
--   Aussehen gematcht wird - also nicht gegen genau ein Blatt als linker Knoten vom Baum z.B. -
--   sondern gegen alles!

--   z.B das wäre ein Problem wenn das die Zeile 14 wäre!

--   dfCollect (Knoten (Blatt c) c' (Blatt c)) = ...


-- ABST)

--   Wir nehmen als Abstiegsfunktion die Höhe des Baumes - welche durch die Anzahl der
--   'Knoten'-Konstruktoren dargestellt sind. Dies ist eine natürliche Zahl!

--   Wir wissen, dass wenn die Anzahl dieser Konstruktoren 0 ist, dann sind wir bei dem Fall
--   '(Blatt c)' angekommen und es finden keine rekursiven Aufrufe statt.

--   Sei diese Anzahl von 'Knoten'-Konstruktoren echt größer als 0
--   Nennen wir t := (Knoten t_l c t_r)
--   Wir kommen dank DEF) sicher in den zweiten Fall.

--   Dann haben wir die Abstiegsfunktion:

--   m :: Baum -> N

--   Da wir die Konstruktoren zählen, ergibt sie offensichtlich die Gleichung

--   m(t) = 1 + m(t_l) + m(t_r)

--   => da m(t) > m (t_l) und
--         m(t) > m (t_r) haben wir eine Funktion gefunden die für jeden rekursiven Aufruf kleiner wird!

