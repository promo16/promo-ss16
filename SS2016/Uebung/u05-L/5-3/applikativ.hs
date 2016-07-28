-- 5-3 a) Auswertung mit applikativen Auswertungsreihenfolge --

-- gegeben:
quadrat = \x -> x * x
summe_quadrate = \x y -> quadrat x + quadrat y
null x = 0

f n = if null (quadrat n) /= n
          then summe_quadrate (n-2) (n-1)
          else n

f = (\n -> if null (quadrat n) /= n
              then summe_quadrate (n-2) (n-1)
              else n)


-- Auswertung nach Call-by-Value (applikative Reihenfolge) von (f 3):

--    f 3                                                        1)
--    ---
--
-- => if null (quadrat 3) /= 3                                   2) Unsere if-then-else Regel greift! Zuerst Bedingung auswerten
--             ---------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if null ((\x -> x * x) 3) /= 3                             3) 
--            -----------------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if null (3 * 3) /= 3                                       4)
--            ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if null 9 /= 3                                             5)
--       ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if 0 /= 3                                                  6)
--       ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if True                                                    7)
--       ----
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => summe_quadrate (3-2) (3-1)                                 8)
--                   -----
--
-- => summe_quadrate 1 (3-1)                                     9)
--                     -----
--
-- => summe_quadrate 1 2                                        10)
--    ------------------
--
-- => (\x y -> quadrat x + quadrat y) 1 2                       11)
--    -----------------------------------
--
-- => quadrat 1 + quadrat 2                                     12)
--    ---------
--
-- => (\x -> x * x) 1 + quadrat 2                               13)
--    ---------------
--
-- => 1 * 1 + quadrat 2                                         14)
--    -----
--
-- => 1 + quadrat 2                                             15)
--        ---------
--
-- => 1 + (\x -> x * x) 2                                       16)
--        ---------------
--
-- => 1 + 2 * 2                                                 17)
--        -----
--
-- => 1 + 4                                                     18)
--    -----
--
-- => 5                                                         19)
--

-- parallele Auswertung:

--    f 3                                                        1)
--    ---
--
-- => if null (quadrat 3) /= 3                                   2) Unsere if-then-else Regel greift! Zuerst Bedingung auswerten
--             ---------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if null ((\x -> x * x) 3) /= 3                             3) 
--            -----------------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if null (3 * 3) /= 3                                       4)
--            ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if null 9 /= 3                                             5)
--       ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if 0 /= 3                                                  6)
--       ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if True                                                    7)
--       ----
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => summe_quadrate (3-2) (3-1)                                 8)
--                   ----- ----
--
-- => summe_quadrate 1 2                                        10)
--    ------------------
--
-- => (\x y -> quadrat x + quadrat y) 1 2                       11)
--    -----------------------------------
--
-- => quadrat 1 + quadrat 2                                     12)
--    ---------   ---------
-- 
-- => (\x -> x * x) 1 + (\x -> x * x) 2                         13)
--    ---------------   ---------------
--
-- => 1 * 1 + 2 * 2                                             14)
--    -----   -----
--
-- => 1 + 4                                                     15)
--    -----
--
-- => 5                                                         16)