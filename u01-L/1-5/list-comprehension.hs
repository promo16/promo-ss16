  

-- Aufgabe 1-5 --
substantive = ["Student", "Professor", "Tutor"]
adjektive   = ["fauler", "fleissiger", "hilfreicher"]

sinnvoll  = [ a ++ " " ++ s | a <- adjektive, s <- substantive]

sinnvoll' = [ a ++ ' ' :  s | a <- adjektive, s <- substantive]

sinnvoll''  = [ (a, s) | a <- adjektive, s <- substantive]

-- Erklärung:
--
-- List Comprehentions sind von der mathematischen Schreibweise motiviert:
--
-- [ a | x <- xs, y <- ys ]
{-
  for(int i = 0; i < length xs; i++){
    x = xs[i]
    for (int j = 0; j < length ys; j++){
      y = ys[j]
      füge a hinzu
    }
  }
-}

-- Eine List Comprehention ist immer gleich aufgebaut, zuerst kommt das Element - a, welches
-- zur entgültigen Liste hinzugefügt wird. Dann kommt der '|' (pipe)-Operator und nach ihm
-- die Bedingungen die erfüllt werden müssen um die Liste zu erstellen.

-- Es gibt eine schöne Schreibweise für jedes Element aus der Liste und wird so geschrieben:

-- x <- xs  (gelesen - für jedes x aus der Liste xs, gilt...)

-- Danach kommen weitere Bedingungen, wie eine bool'scher Ausdruck, oder eine weiter Liste.

-- x <- xs, y <- ys (gelesen - für jedes x aus der Liste xs gilt, für jedes y <- ys gilt)

-- Was wir jetzt bekommen haben ist das Kreuzprodukt zwischen xs und ys, oder 'jeder mit jedem'

-- Weitere Beispiele:

-- [x | x <- [1,2,3]] => [1, 2, 3]

-- [x | x <- [1,2,3], y <- ['a','b']] => [1,1,2,2,3,3]
-- Wir haben hier für jedes y ein ein x in die Liste reingetan

-- [(x,y) | x <- [1,2,3], y <- ['a','b']] => [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

-- [1 | x <- [1..3]] => [1, 1, 1]

-- [x | x <- [1..3], x `mod` 2 == 0] => [2]

-- [1 | x <- [1..3], (x `mod` 2 == 0 || x == 1)] => [1, 1]

-- [x | x <- [1..3], (x `mod` 2 == 0 || x == 1)] => [1, 2]
