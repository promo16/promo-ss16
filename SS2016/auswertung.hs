import Prelude hiding (const)

-- Weitere Erklärungen in u05-L und u06-L

-- Auswertungsbeispiel
--

add1 :: Int -> Int
add1 x = x + 1

const :: a -> b -> a
const x y = x

fun :: Int
fun = let a = 10
      in  const (add1 3) (const a (7-2))

-- | Erinnerung:
--
--  *) die Funktionen immer zuerst in die Lambda-Form bringen
--  *) let-Umgebung und das entfernen der Variablen nach dem Auswerten nicht vergessen
--  *) let-Umgebung von vorne befüllen!
--  *) bei 'if-then-else' immer zuerst das Prädikat auswerten, egal welche Auswertungsreihenfolge
--
-- | Applikative Reihenfolge:
--
-- *) innere Ausdrücke zuerst
-- *) parallele Auswertung wenn möglich, ansonsten von links nach rechts

-- | Normale Reihenfolge:
--
-- *) äußere Ausdrücke zuerst
-- *) parallele Auswertung wenn möglich, ansonsten von links nach rechts

-- | Verzögerte Reihenfolge:
--
-- *) Genauso wie Normale Reihenfolge, man muss folgendes beachten
--   **) jeden Ausdruck bis auf den äußersten mit 'let' in die Umgebung mit Namen (Referenz) packen

--           => foo (f 1) (g 2 3)               []
--           => let a = f 1
--                  b = g 2 3
--              in f a b                        []
--           => f a b                           [(b, g 2 3), (a, f 1)]

--   **) beim Einsetzen der Referenzen diese mit '^' versehen. (e.g  (\x -> x + 1) a => ^a + 1)
--   **) beim Ausrechnen der Referenzen Zwischenrechnungen verschoben hinschreiben (oder anderweitig zeigen, dass es nicht die Hauptevaluation ist)
--         => ^a + 1                            [(a, 1 + 1)]
                          
--                   => a                       [(a, 1 + 1)]
--                   => 1+1                     [(a, 1 + 1)]
--                   => 2                       [(a, 1 + 1)]
--         =>  2 + 1

--
--   Diese beiden "Regeln" sind nicht Pflicht, geben aber dem ganzen eine Struktur, weil es nicht Vorlesung spezifiziert wurde.
--   Die zweite Lösung macht es einfach undefiniert sollte aber dennoch völlig richtig sein
-- 
--   **) nach dem Ausrechnen der Referenz, das neue Ergebnis in die Umgebung reinschreiben
--         => e.g         ^x                      [(x, 1 + 1)]
--                        1 + 1                   [(x, 1 + 1)]
--                        2                       [(x, 2)]

--   **) nach Ausrechnen der Referenz die Umgebung von neuen let-Ausrücken leeren 
--          => e.g        ^x                      [(x, fun 10)])
--                        fun 10
--                        let y = 10 in fun y     [(y, 10), (x, fun 10)]
--                        (fun y ausrechnen...)   [(y, 10), (x, fun 10)]
--                        20                      [(y, 10), (x, fun 10)]
--                                                [(x, 20)]


-- | Aufgabe:
--
--  Werten sie die Funktion 'fun' in applikativer, normaler und verzögerter Reihenfolge  aus:
--
--

-- Schritt 1:
--    Erstmal schreiben wir alle Funktionen in die Lambda-Form um:

add1_ :: Int -> Int
add1_ = \x -> x + 1

const_ :: a -> b -> a
const_ = \x y -> x

--    Da 'fun' keine Argumente nimmt, ist hier nichts zu tun.

-- #################################################################################################
-- #################################   Applikative Auswertung ######################################
-- #################################################################################################

--                                                        Umgebung:
      fun                                               --   []
     -----

=>  let a = 10 in const (add1 3) (const a (7-2))        --   []
    --------------
       Let-Regel

=>  const (add1 3) (const a (7-2))                      --   [(a,10)]
          -------           -----
            |                  |
       innerser               innerster
       Ausdruck in der        Ausdruck in der
       linken Klammer         rechten Klammer


=>  const ((\x -> x + 1) 3) (const a 5)                 -- [(a,10)]
           ----------------  ----------

=>  const (3 + 1) ((\x y -> x) a 5)                     -- [(a,10)]
          ------   ---------------                       
                          |
                  Um das zu evaluieren müssen wir
                  zuerst in der Umgebung nach 'a' suchen
                  und einsetzen

=>  const 4 ((\x y -> x) 10 5)                           -- [(a,10)]
             -----------------

=>  const 4 10                                           -- [(a,10)]
    ----------

=>  (\x y -> x) 4 10                                     -- [(a,10)]

=>  4                                                    -- [(a,10)]

   Nicht vergessen die Umgebung nach dem Auswerten des Ausdrucks zu bereinigen

=>                                                       -- []



-- #################################################################################################
-- #################################### Normale Auswertung #########################################
-- #################################################################################################


    fun                                                 -- []
    ---

=>  let a = 10 in const (add1 3) (const a (7-2))        -- []
    -------------

=>  const (add1 3) (const a (7-2))                      -- [(a,10)]
    ------------------------------
        Der äußerste Ausdruck

=>  (\x y -> x) (add1 3) (const a (7-2))                -- [(a,10)]
    ------------------------------------

=>  add1 3                                              -- [(a,10)]
    ------

=>  (\x -> x + 1) 3                                     -- [(a,10)]
    ---------------

=>  4                                                   -- [(a,10)]

=>                                                      -- []


-- #################################################################################################
-- ################################## Verzögerte Auswertung ########################################
-- #################################################################################################


    fun                                                 -- []
    ---

=>  let a = 10 in const (add1 3) (const a (7-2))        -- []
    -------------

=>  const (add1 3) (const a (7-2))                      -- [(a,3)]
           ------   -------------
              \         /
            Referenzen verteilen (Namen beliebig)

=> let b = add1 3
       c = const a (7-2)
   in const b c                                         -- [(a,3)]

   In die Umgebung aufnehmen

=>  const b c                                           -- [(c, const a (7-2)), (b, add1 3), (a,3)]
    ---------

=> (\x y -> x) b c                                      -- [(c, const a (7-2)), (b, add1 3), (a,3)]
   ---------------

=> ^b                                                   -- [(c, const a (7-2)), (b, add1 3), (a,3)]
   ---
      => b
      => add1 3
      => let d = 3
         in add1 d                                      -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

      => (\x -> x + 1) d                                -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]
      => ^d + 1                                         -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

          => d                                          -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]
          => 3                                          -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

      => 3 + 1                                          -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

      => 4                                              -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]
      =>                                                -- [(c, const a (7-2)), (b, 4), (a,3)]

=> 4                                                    -- [(c, const a (7-2)), (b, 4), (a,3)]
=>                                                      -- []


-- Zweite Lösung (ohne die letzten beiden Regeln):

    fun                                                 -- []
    ---

=>  let a = 10 in const (add1 3) (const a (7-2))        -- []
    -------------

=>  const (add1 3) (const a (7-2))                      -- [(a,3)]
           ------   -------------
              \         /
            Referenzen verteilen (Namen beliebig)

=> let b = add1 3
       c = const a (7-2)
   in const b c                                         -- [(a,3)]

   In die Umgebung aufnehmen

=>  const b c                                           -- [(c, const a (7-2)), (b, add1 3), (a,3)]
    ---------

=> (\x y -> x) b c                                      -- [(c, const a (7-2)), (b, add1 3), (a,3)]
   ---------------

=> ^b                                                   -- [(c, const a (7-2)), (b, add1 3), (a,3)]
   ---
      => b
      => add1 3
      => let d = 3
         in add1 d                                      -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

      => (\x -> x + 1) d                                -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]
      => ^d + 1                                         -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

          => d                                          -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]
          => 3                                          -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

      => 3 + 1                                          -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]

      => 4                                              -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]   <---\
      =>                                                -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]   <-  - Hier wird die Umgebung einfach gelassen 
                                                        --                                                      /
=> 4                                                    -- [(d,3), (c, const a (7-2)), (b, add1 3), (a,3)]   <-/
=>                                                      -- []

