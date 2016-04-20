module Main where


-- | Die Musterlösung (in der .pdf) enthält einen Tippfehler in der 12-4
--   'ist Teilmenge von' sollte eigentlich [= sein


-- A12-1 Hasse Diagramm (alte Aufgabe)

-- Die Aufgabe war hier ein Hesse-Diagramm für den Datentyp:

-- a)

-- data Entweder = EinB  Bool
--               | Azahl Int

-- Das Beispiel für 'Maybe Bool':

--                  Just True   Just False
--                         \     /
--    Nothing               \   /
--        \                  \ /
--         \               Just _|_
--          \                 /
--           \   ____________/
--            \ /
--            _|_

-- _|_ - Dieses Zeichen nennen wir 'bottom'


-- b)

-- Wir schauen wie wir ein sog. 'thunk' auswerten - und was für eine Form er
-- annehmen kann:

-- x1 = undefined
-- x2 = Nothing
-- x3 = Just undefined
-- x4 = Just True
-- x5 = Just False


-- y1 = undefined
-- y2 = EinB undefined
-- y3 = Azahl undefined
-- y4 = EinB True
-- y5 = EinB False
-- y6 = Azahl 0
-- y7 = Azahl 1
-- y8 = Azahl (-1)
-- y9 = Azahl 2
--y10 = Azahl ...

--      EinB True   EinB False      Azahl 0  Azahl 1 Azahl -1  Azahl 2    ...
--            \     /                    \      /       /         /       /
--             \   /                      \    /       /         /       /
--              \ /                        \  /_______/_________/_______/
--               |                          \//
--            EinB _|_                   Azahl _|_
--                 \           _____________/
--                  \         /
--                   \       /
--                    \     /
--                     \   /
--                      _|_



-- c) [Bool]


-- Leider etwas unübersichtlich, dsw lieber selber abmalen.

-- 2a -> 3a, 3b

-- 2b -> 3a, 3c

-- 2c -> 3b, 3d

-- 2d -> 3c, 3d, 3e, 3f, 3g, 3h


--   3a)                 3b)                    3c)                   3d)                    3e)                      3f)                 3g)                    3h)
-- True : []         False : []          True : _|_ : _|_      False  : _|_ : _|_      _|_ : True : _|_      _|_ : False : _|_      _|_ : _|_ : []      _|_ : _|_ : _|_ : _|_


--    2a)                 2b)                2c)                    2d)
-- _|_ : []          True : _|_          False : _|_           _|_ : _|_ : _|_
--     |__________________|___________________|__________________________|
--                                    \____________________________
--                                                                 |
-- []                                                          _|_ : _|_
-- |                                                               |
-- |                                                               |
--  \______________________________________________________________|
-- _|_


-- | A12-2 Approximation rekursiver Funktionen

-- f n = if n == 0
--          then 1
--          else n + f (n - 1)


-- Wertetabelle wie in der Vorlesung (?)

--        0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |
-- f_0 | _|_ | _|_ | _|_ | _|_ | _|_ | _|_ | _|_ | _|_ |
-- f_1 |  0  | _|_ | _|_ | _|_ | _|_ | _|_ | _|_ | _|_ |
-- f_2 |  0  |  1  | _|_ | _|_ | _|_ | _|_ | _|_ | _|_ |
-- f_3 |  0  |  1  |  3  | _|_ | _|_ | _|_ | _|_ | _|_ |
-- f_4 |  0  |  1  |  3  |  6  | _|_ | _|_ | _|_ | _|_ |
-- f_5 |  0  |  1  |  3  |  6  |  10 | _|_ | _|_ | _|_ |
-- f_6 |  0  |  1  |  3  |  6  |  10 |  15 | _|_ | _|_ |
-- f_7 |  0  |  1  |  3  |  6  |  10 |  15 |  21 | _|_ |



-- in der Wiki-Page wird f_k so definiert

-- f_(k + 1) n = if n == 0 then 0 else n + f_k (n - 1)

-- Aufgabe - definiere f_0, f_1, f_2, f_3, f_4

-- f_0 n = _|_

f_0 n = undefined

--    f_1 n = if n == 0 then 0 else n + f_0 (n - 1)
-- => f_1 n = if n == 0 then 0 else n + undefined
-- => f_1 n = if n == 0 then 0 else undefined

f_1 0 = 0
f_1 n = undefined


--    f_2 n = if n == 0 then 0 else n + f_1 (n-1)
-- => f_2 n = if n == 0 then 0 else n + (if (n-1) == 0 then 0 else undefined)

f_2 0 = 0
f_2 1 = 1
f_2 n = undefined

--    f_3 n = if n == 0 then 0 else n + f_2 (n-1)
-- => f_3 n = if n == 0 then 0 else n + (if (n-1) == 0 then 0 else (n-1) + f_1 (n-2))
-- => f_3 n = if n == 0 then 0 else n + (if (n-1) == 0 then 0 else (n-1) + if (n-2) == 0 then 0 else undefined

f_3 0 = 0
f_3 1 = 1
f_3 2 = 3
f_3 n = undefined

-- analog die f4 und f5

f_4 0 = 0
f_4 1 = 1
f_4 2 = 3
f_4 3 = 6
f_4 n = undefined

f_5 0 = 0
f_5 1 = 1
f_5 2 = 3
f_5 3 = 6
f_5 4 = 10
f_5 n = undefined

g :: (Integer -> Integer) -> Integer -> Integer
g f 0 = 0
g f n = n + f (n - 1)

test_0 = take 10 $ iterate (g f_0) 0
-- [0,0,0,0,0,0,0....]

test_1 = take 10 $ iterate (g f_1) 1
-- [1,1,1,1,1,1,1....]

test_2 = iterate (g f_2) 2
-- [2,3, *** Exception: Prelude.undefined]

test_3 = iterate (g f_3) 3
-- [3,6, *** Exception: Prelude.undefined]

test_4 = iterate (g f_4) 4
-- [4,10, *** Exception: Prelude.undefined]

test_5 = iterate (g f_5) 5
-- [5,15, *** Exception: Prelude.undefined]


-- b) McCarthy-91-Funktion

mc91 :: Integer -> Integer
mc91 n | n > 100   = n - 10
       | otherwise = mc91 $ mc91 $ n + 11


-- versuchen wir die Funktion zu verstehen:

-- bei 101 brauchen wir keinen rekursiven Aufruf! -> 91

-- bei 100 brauchen wir einen zwei verschachtelte rekursive Aufrufe!
--   => m91 $ m91 100 + 11
--   => m91 $ 111 - 10
--   => m91 $ 101
--   => 101 - 10
--   => 91

-- bei 99 brauchen wir vier verschachtelte rekursive Aufrufe!
--   => m91 $ m91 $ 99 + 11             die ersten beiden Aufrufen!
--   => m91 $ 110 - 10
--   => m91 $ 100
--   => m91 $ m91 $ 100 ++ 11           die letzten beiden Aufrufe!
--   => m91 $ 111 - 10
--   => m91 $ 101
--   => 101 - 10
--   => 91

-- analog geht es weiter...


mcf_0 n = undefined

mcf_1 n | n > 100   = n - 10
        | otherwise = mcf_0 $ mcf_0 $ n + 11

mcf_2 n | n >  100  = n - 10
        | otherwise = mcf_1 $ mcf_1 $ n + 11

mcf_3 n | n >  100  = n - 10
        | otherwise = mcf_2 $ mcf_2 $ n + 11

-- ...
-- ...
-- ...

-- Setzen wir ein!

mcf_0' n = undefined

-- mcf_1 n | n > 100   = n - 10
--          | otherwise = mcf_0' $ mcf_0' $ n + 11

-- da wir wissen dass wenn wir zu viele rekursive Aufrufe von der Funktion verlangen, sie nach
-- undefined auflöst - setzen wir auch wie in der a) undefined rein!

mcf_1' n | n > 100   = n - 10
         | otherwise = undefined

mcf_2' n | n >  100  = n - 10
         | n >= 100  = 91
         | otherwise = undefined

mcf_3' n | n >  100  = n - 10
         | n >=  99  = 91
         | otherwise = undefined

mcf_4' n | n >  100  = n - 10
         | n >=  98  = 91
         | otherwise = undefined

mcf_5' n | n >  100  = n - 10
         | n >=  97  = 91
         | otherwise = undefined

mcf_6' n | n >  100  = n - 10
         | n >=  96  = 91
         | otherwise = undefined

-- ...
-- ...
-- ...

-- schauen wir uns den 11-ten und 12-ten Fall etwas genauer an:

mcf_11' n | n > 100   = n - 10
          | n >= 91   = 91
          | otherwise = undefined


-- Fall 11

--    mc91 91
-- => mc91 $ mc91 $ 91 + 11
-- => mc91 $ mc91 $ 102
-- => mc91 $ 102 - 10
-- => mc91 $ mc91 $ 92 + 11
-- ...

-- Fall 12

--    mc91 90
-- => mc91 $ mc91 $ 90 + 11
-- => mc91 $ mc91 $ 101
-- => mc91 $ 101 - 10
-- => mc91 $ 91
-- => mc91 $ 91 + 11
-- => mc91 $ mc91 $ 102
-- ...

-- Hier addieren wir zwei mal 11 dazu - das heißt, die Definiertheit der iterierten
-- partiellen Funktion steigt ab jetzt in 11er Schritten ab.

-- In anderen Worten, ob wir nun 85 oder 89 einsetzen, wir müssen uns so oder so
-- zwei mal rekursiv aufrufen und zwei mal eine 11 dazu addieren. Damit decken wir
-- eine größere Definiertheit ab!

-- Die Schlussfolgerung daraus ist, dass wir ab jetzt in 11er Schritten runtergehen!

mcf_12' n | n > 100   = n - 10
          | n >= 80   = 91
          | otherwise = undefined

mcf_13' n | n > 100   = n - 10
          | n >= 69   = 91
          | otherwise = undefined



-- Nun definieren wir uns wieder eine FixPunkt

mcg x n
  | n > 100   = n - 10
  | otherwise = x $ x $ n + 11


mcg_test = iterate (mcg mcf_13') 79 -- funktioniert bis genau 79 und nicht weiter!



-- | A12-3


-- Was bedeutet [= ?

-- x [= y bedeutet dass y gleich gut, oder mehr definiert ist als x
--   (Das sind die Pfeile die wir im Hasse-Diagramm ziehen)

-- Das ist eine partielle Ordnung, die die folgenden Gesetzen unterworfen ist:

--  Transitivität  (in A11_L erklärt)
--  Antisymmetrie  (in A11_L erklärt)
--  Reflexivität   (in A11_L erklärt)

-- Bsp.

--  _|_ [= 1
--  _|_ [= Just _|_ [= Just 4

--  1 [= 2 ist FALSCH!

-- 1  _2  _3
--  \/___/
--   \
--  _|_

-- => _|_ [= 2

-- 1 [= 1 stimmt aber, da wir hier die Gleichheit mit überprüfen

-- Was bedeutet [ ?

-- Es ist ein semantischer Approximations-Ordnungs-Operator.
-- Man kann dadurch sagen, ob ein Datentyp mehr oder weniger definiert ist.

-- x [ y bedeutet, dass y sicherlich mehr definiert ist als x!

-- Bsp.

--  1 [ 2 ist falsch, da wir hier gleich definierte Daten haben

--  Just _|_ [ Just 5

--  _|_ [ Just (Just 4)



-- TODO: Definiere 'g' sodass

fun = iterate g' undefined

-- zu einer Kette von partiell definierten Werten auswertet:
-- [undefined, True : False : undefined,  True : False : True : False : undefined,...]

-- Semantische Sicht:

-- _|_  [= True : False : _|_ [= True : False : True : False : undefined [=...

g' :: [Bool] -> [Bool]
g' xs = True : False : xs

-- Beispielhafter Durchlauf:

-- iterate g' undefined

-- g' undefined => True : False : undefined

-- [undefined, iterate g' (True : False : undefined)]

-- g' (True : False : undefined) => True : False : True : False : undefined

-- [undefined, True : False : undefined, iterate g' (True : False : True : False : undefined)]

-- ...

exp0 = fun !! 0
exp1 = fun !! 1
exp2 = fun !! 2
exp3 = fun !! 3



-- | A12-4
--
-- Was ist ein DCPO?
--
-- Eine 'directed complete partial order' ist eine poset das ein supremum besitzt
--
-- Was ist ein Supremum?

-- Ein Element, welches in einer Kette entweder größer oder gleich zu allen Elementen
-- im Verhältnis steht - es ist definiert als kleinste obere Schranke!
--
--
-- Sei
--
--   - (X, [=) eine DCPO
--   - x_i € X eine Kette ( x_0 [= x_1 [= x_2 [= x_3 [=... )
--   - Supremum von x_i nennen wir sup_i (x_i)
--
--   - eine andere Kette y_i im gleichen DCPO, wo für jedes i € N gilt
--     x_i = y_(i + 1) = y_j
--
-- Zu beweisen:
--
--   - sup_i (x_i) = sup_j (y_j)

-- Wie sehen diese Ketten aus?

--        x_0 [= x_1 [= x_2 [= ... [= x_i
--          \      \      \        .     \____
--           \      \      \        .         \
--            \      \      .        \         \
--             \      \      .        \         \
--      y_0 [= y_1 [= y_2 [= ... [= y_(j-1) [= y_j


-- => y_0 wir unten angehängt!

-- => die Kette ist identisch, nur dass von unten ein zusätzliches Element
--    angehängt wurde und dadurch die Länge verändert wurde


-- Wir erinnern uns an Anti-Symmetrie:

-- x R y && y R x <=> x = y

-- genau das können wir anwenden um die Gleichheit den suprema von x_i und y_j zu zeigen

-- Fall 1: sup_j (y_j) [= sup_i (x_i)

    -- Wir müssen also für alle j € N zeigen, dass y_j [= sup_i (x_i) stimmt.

    -- Für j > 0 können wir uns den Beweis sparen, da y_j durch x_(i-1) definiert ist:

    --   y_j = x_(j - 1)

    -- Daher können wir schlussfolgern, dass

    --   y_j [= sup_i (x_i)           , da

    --   x_j [= sup_i (x_i)

    -- Aber uns fehlt nun noch der Fall j = 0 (das ist das hinzugefügte Element):

    -- Der ist zum Glück einfach durch Transitivität zu beweisen. Diese ist definiert als

    --   x R y && y R z <=> x R z

    -- Genau das brauchen wir hier ja auch - wir müssen zeigen, dass

    --   y_0 [= sup_i (x_i)

    -- Wir wissen, dass y_0 [= y_1, weil y_j eine Kette ist!

    -- Wir wissen auch, dass y_1 = x_0

    -- Und zuletzt wissen wir auch, dass x_0 [= sup_i (x_i)

    -- Durch die Transitivität gilt offensichtlich auch

    --   y_0 [= y_1 = x_0 [= sup_i (x_i)


-- Fall 2 - sup_i (x_i) [= sup_j (y_j):

    -- Wir wissen ja, dass für alle i € N gilt - x_i = y_(i + 1)

    -- Daher können wir herleiten, dass x_i [= sup_j (y_j)!

    -- Das heißt, dass das supremum vom der y_j eine obere Schranke von x_i ist

    -- Aus der Definition von suprema wissen wir, dass sup_i (x_i) aber die KLEINSTE
    -- obere Schrank ist. => Daraus schlussfolgern wir dass sup_i (x_i) [= sup_j (y_j) ist!

    -- Damit haben wir bewiesen dass beide Schranken wirklich identisch sind! q.e.d.