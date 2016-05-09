
-- 4-2 Sieb des Eratosthenes --

sieve :: Integral a => [a] -> [a]
sieve []     = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- Erklärung:
--
-- Wir haben es hier zum Glück mit einer Liste der festen Länge zutun - damit wird die Aufgabe gleich
-- viel einfacher.
--
-- Wenn wir wissen das unsere Liste endlich ist, können wir mit jeder Zahl die wir rausgeben, einfach
-- alle Zahlen rausschmeißen, die diese Zahl teilt. Das machen wir wie oben gezeigt durch eine List-Comprehention
--
-- Beispielevaluation:
--
-- sieve [2..10]
-- => sieve (2:xs) = 2 : sieve [y | y <- xs,      y `mod` x /= 0]
--                   2 : sieve [y | y <- [3..10], y `mod` x /= 0]
--                              3  `mod` 2 == 1 (passt)
--                              4  `mod` 2 == 0 (wird rausgeschmissen)
--                              5  `mod` 2 == 1 (passt)
--                              6  `mod` 2 == 0 (wird rausgeschmissen)
--                              7  `mod` 2 == 1 (passt)
--                              8  `mod` 2 == 0 (wird rausgeschmissen)
--                              9  `mod` 2 == 1 (passt)
--                              10 `mod` 2 == 0 (wird rausgeschmissen)
--                   2 : sieve [3,5,7,9]
--                   2 : sieve (3:xs) => 2 : 3 : sieve [y | y <- [5,7,9], y `mod` x /= 0]
--                                                      5 `mod` 3 == 2 (passt)
--                                                      7 `mod` 3 == 1 (passt)
--                                                      9 `mod` 3 == 0 (wird rausgeschmissen)
--                                    => 2 : 3 : sieve [5,7]
--                                    => 2 : 3 : 5 : [y | y <- [7], y `mod` x /= 0]
--                                                    7 `mod` 5 = 2 (passt)
--                                    => 2 : 3 : 5 : 7 : sieve []
--                                    => 2 : 3 : 5 : 7 : []
--                                    => [2, 3, 5, 7]
--