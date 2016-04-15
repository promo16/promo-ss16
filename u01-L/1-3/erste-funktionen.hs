
-- Aufgabe 1-3 a --
dreifach :: Integer -> Integer
dreifach   x = x * 3

dreifach'  x = x + x + x


-- Aufgabe 1-3 b --
vierfach :: Integer -> Integer
vierfach   x = x + dreifach x

vierfach'  x = x * 4

vierfach'' x = x + x + x + x