-- Aufgabe 9-2 "Mathematische Terme als Bäume"
--

-- a) Geben sie einen geeigneten Datentyp für Abstract-Syntax-Trees (AST's) der folgende Operationen unterstüzt:

-- *) Addition
-- *) Subtraktion
-- *) Multipliktation
-- *) Division

-- gegeben:

-- BinaryTerm Times
--    (BinaryTerm Plus  (C 5) (C 6))
--    (BinaryTerm Minus (C 3) (C 1))

-- Binary Operator also eine Funktion die zwei Argumente nimmt
data BinOP = Plus
           | Minus
           | Times
           | Division
    deriving Show

-- Unary Operator, also eine Funktion die ein Argument nimmt
data UnOP  = Negate   -- c)
    deriving Show

data Term a = BinaryTerm BinOP (Term a) (Term a)  -- a) -- Arithmetischer Ausdruck der einen binären Operator und zwei Terme als Argument nimmt
            | C a                                 -- a) -- Konstante
            | UnaryTerm  UnOP  (Term a)           -- c) -- Arithmetischer Ausdruck der einen unären Operator und ein Term als Argument nimmt

-- Wie man sieht ist die Definition extrem ähnlich zu den Binärbäumen.

-- b) implementieren sie die Funktion 'eval :: Num a => Term a -> a'. Hier müsste eigentlich 'Integral a =>' stehen, weil wir nur
--    Ganzkommadivision erlauben

eval :: Integral a => Term a -> a
eval (C n)                        = n
eval (BinaryTerm Plus      t1 t2) = eval t1   +   eval t2
eval (BinaryTerm Minus     t1 t2) = eval t1   -   eval t2
eval (BinaryTerm Times     t1 t2) = eval t1   *   eval t2
eval (BinaryTerm Division  t1 t2) = eval t1 `div` eval t2


-- Hier eignet sich ein case-Patternmatch, weil wir dann weniger Schreibarbeit haben (wir wissen ja bereits wie unsere Funktion heißt,
-- wir müssen sie nicht jede Zeile wiederholen)
--
eval' :: Integral a => Term a -> a
eval' (C n)                 = n
eval' (BinaryTerm op t1 t2) =
    case op of
        Plus     -> eval' t1   +   eval' t2
        Minus    -> eval' t1   -   eval' t2
        Times    -> eval' t1   *   eval' t2
        Division -> eval' t1 `div` eval' t2


-- c) Erlauben sie das negieren von Zahlen mit einem neuen Operator.

-- Hier wurde recht wenig spezifiziert, aber wenn man seinen Code erweiterbar halten will und nicht unbedingt von Zahlen ausgeht,
-- dann kann es sehr gut mehrere unäre Operatoren geben. Daher bietet es sich an einen neuen Datentyp 'UnOP' zu erstellen und einen
-- neuen Konstruktor in 'Term' hinzuzufügen

eval'' :: Integral a => Term a -> a
eval'' (C n)                        = n
eval'' (BinaryTerm Plus      t1 t2) = eval'' t1   +   eval'' t2
eval'' (BinaryTerm Minus     t1 t2) = eval'' t1   -   eval'' t2
eval'' (BinaryTerm Times     t1 t2) = eval'' t1   *   eval'' t2
eval'' (BinaryTerm Division  t1 t2) = eval'' t1 `div` eval'' t2
eval'' (UnaryTerm  Negate    t)     = - (eval'' t)


eval''' :: Integral a => Term a -> a
eval''' (C n) = n
eval''' (UnaryTerm Negate t) = - (eval''' t)
eval''' (BinaryTerm op t1 t2) =
    case op of
        Plus     -> eval''' t1   +   eval''' t2
        Minus    -> eval''' t1   -   eval''' t2
        Times    -> eval''' t1   *   eval''' t2
        Division -> eval''' t1 `div` eval''' t2


-- Das Beispiel (5 + (- 3)) * (3 - 1) würde dann so ausschauen:

-- BinaryTerm Times (BinaryTerm Plus
--                                   (C 5))
--                                   (UnaryTerm Negate 3))
--                  (BineryTerm Minus
--                                   (C 3)
--                                   (C 1))

-- d) implementieren sie einen Termvereinfacher der folgende Regeln benutzt:

-- 1) -(-n) = n
-- 2) x + (- y) = x - y
-- 3) x - (- y) = x + y

-- Hier wurde leider nicht gesagt, wie viele sog. 'passes' wir über unseren AST machen sollen, wir nehmen einfach an, es ist nur einer
--
simplify :: Term a -> Term a
simplify (UnaryTerm  Negate    (UnaryTerm Negate t) ) = simplify t                                   -- 1)
simplify (BinaryTerm Plus   t1 (UnaryTerm Negate t2)) = BinaryTerm Minus (simplify t1) (simplify t2) -- 2)
simplify (BinaryTerm Minus  t1 (UnaryTerm Negate t2)) = BinaryTerm Plus  (simplify t1) (simplify t2) -- 3)
simplify (BinaryTerm op     t1 t2                   ) = BinaryTerm op    (simplify t1) (simplify t2)
simplify (UnaryTerm  op     t                       ) = UnaryTerm  op    (simplify t)
simplify (C n)                                        = C n

-- Durch Pattern-Matching können wir die Regeln förmlich 1-zu-1 abschreiben 
