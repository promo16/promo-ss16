-- | Aufgabe 7-4 - "Abstiegsfunktionen"
--
--
-- gegeben:

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

-- z.z Termination durch eine Abstiegsfunktion

-- AUF) Führen die rekursiven Aufrufe aus der Definitionsmenge heraus?
--
--      Die Definitionsmenge ist in unserem Fall definiert als polymorphe Liste '[a]'. Wir rufen uns lediglich
--      in Z.7 rekursiv auf mit dem Rest einer Liste, die bereits eine valide Eingabe war.
--
--      => Nein, also passt alles.

-- DEF) Werden alle möglichen Argumente für unsere Menge abgefangen?
--
--      Es sind jeweils zwei Fälle definiert - Z.6 ~ die Liste ist leer und ~ Z.7 die hat mindestens ein Element.
--      Damit sind alle möglichen Fälle abgefangen
--
--      => Ja, also passt alles.

-- ABST) Finde eine Abstiegsfunktion die für jeden rekursiven Aufruf ECHT kleiner wird.
--
--      Wir haben nur einen rekursiven Aufruf in Z.7. Schreiben wir die Gleichung hin die erfüllt werden muss:
--
--      gesucht ist ein 'f' das folgende Eigenschaft erfüllt:
--
--          length' (x:xs) => f (x:xs) < f (xs) <= lenght' xs
--
--      Bei Funktionen die Listen nehmen ist es meistens eine gute Idee über die Länge bzw. Kardinalität der Liste zu gehen.
--      Wir wählen uns f (l) = | l | und setzen ein:
--
--      Sei beliebig |xs| = n
--
--          length' (x:xs) => f (x:xs) = | (x:xs) | = 1 + n > n = | xs | = f (xs) <= length' xs
--
--      Damit haben wir eine Abstiegsfunktion gefunden die für ALLE (in diesem Fall einer) rekursiven Aufrufe immer kleiner wird.
--

-- AUF) & DEF) & ABST) => Die Funktion length' terminiert immer (wenn wir die Definitionsmenge auf endliche Listen begrenzen)



