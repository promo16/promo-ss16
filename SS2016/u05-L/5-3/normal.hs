-- 5-3 b) Auswertung mit normalen Auswertungsreihenfolge --

-- gegeben:
quadrat = \x -> x * x
summe_quadrate = \x y -> quadrat x + quadrat y
null x = 0
f n = if null (quadrat n) /= n
          then summe_quadrate (n-2) (n-1)
          else n

-- Auswertung nach Call-by-Name von (f 3) (links -> rechts):

--    f 3                                                        1)
--    ---
--
-- => if null (quadrat 3) /= 3                                   2) Unsere if-then-else Regel greift! Zuerst Bedingung auswerten
--       --------------- 
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if 0 /= 3                                                  3)
--       ------
--          then summe_quadrate (3-2) (3-1)
--          else 3
--
-- => if True                                                    4)
--    -------
--          then summe_quadrate (3-2) (3-1)
--          -------------------------------
--          else 3
--          ------
--
-- => summe_quadrate (3-2) (3-1)                                 5)
--    --------------------------
--
-- => (\x y -> quadrat x + quadrat y) (3-2) (3-1)                6)
--    -------------------------------------------
--
-- => quadrat (3-2) + quadrat (3-1)                              7)
--    ------------ 
--
-- => (\x -> x * x) (3-2) + quadrat (3-1)                        8)
--                          -------------
--
-- => (\x -> x * x) (3-2) + (\x -> x * x) (3-1)                  9)
--    -------------------
--
-- => (3-2) * (3-2) + (\x -> x * x) (3-1)                       10)
--                    -------------------
--
-- => (3-2) * (3-2) + (3-1) * (3-1)                             11)
--    -----
--
-- => 1 * (3-2) + (3-1) * (3-1)                                 12)
--        -----
--
-- => 1 * 1 + (3-1) * (3-1)                                     13)
--            -----
--
-- => 1 * 1 + 2 * (3-1)                                         14)
--                -----
--
-- => 1 * 1 + 2 * 2                                             15)
--    -----
--
-- => 1 + 2 * 2                                                 16)
--        -----
--
-- => 1 + 4                                                     17)
--    -----
--
-- => 5
--



