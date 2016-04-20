module Expr where

import Prelude hiding (lex)
import Data.Char
import Data.Maybe

data Expr = Const Integer
          | Plus  Expr Expr
          | Times Expr Expr
  deriving (Eq)

instance Show Expr where
    show (Const i)     = show i
    show (Plus  e1 e2) = "("++ show e1 ++ "+" ++ show e2 ++")"
    show (Times e1 e2) = "("++ show e1 ++ "*" ++ show e2 ++")"

instance Read Expr where
    readsPrec _ s = let (e,t) = parseExpr $ lex s in [(e,concatMap show t)]

-- Beispiel:
a2 = Times (Plus (Const 5) (Const 3)) (Const 2)

eval :: Expr -> Integer
eval (Const     n)  = n
eval (Plus  l r)    = eval l + eval r
eval (Times l r)    = eval l * eval r

data Token
  = CONST Integer  -- irgendeine Zahl
  | LPAREN         -- linke Klammer
  | RPAREN         -- rechte Klammer
  | PLUS           -- (+)
  | TIMES          -- (*)

instance Show Token where
    show (CONST i) = show i
    show LPAREN  = "("
    show RPAREN  = ")"
    show PLUS    = "+"
    show TIMES   = "*"


-- Beispiel:
s1 = [CONST 3, TIMES, LPAREN, CONST 8, PLUS, CONST 3, RPAREN, PLUS, CONST 5, TIMES, CONST 4]


-- Lexikalische Analyse
lex :: String -> [Token]
lex  ""      = []
lex ('(':xs) = LPAREN : lex xs
lex (')':xs) = RPAREN : lex xs
lex ('+':xs) = PLUS   : lex xs
lex ('*':xs) = TIMES  : lex xs
-- Z.55/56 sind identisch
lex ( c :xs) | isSpace c = lex xs
lex (' ':xs)             = lex xs

-- Z.60-62 sind identisch zu 64-65
lex xs = case lexInt xs of
             Just (i, ys) -> CONST i : lex ys
             (_)          -> error "Unbekanntes Token!"

-- eine Möglichkeit zur Benutzung von Pattern-Guards
-- lex xs       | Just (i, ys) <- lexInt xs = CONST i : lex ys
-- lex xs        = error $ "Unbekanntes Token: " ++ xs



lexInt :: String -> Maybe (Integer, String)
lexInt s | null num  = Nothing
         | otherwise = Just (read num, rest)
  where
    (num,rest) = span (`elem` '-':['0'..'9']) s

-- Variante
lexInt1 :: String -> Maybe (Integer, String)
lexInt1 = listToMaybe . reads

-- Natürlich könnte man das auch komplett zu Fuss implementieren:
-- lexInt2 ('0':s) = ...
-- lexInt2 ('1':s) = ...
--  ...

parseExpr :: [Token] -> (Expr, [Token])
parseExpr l  = let (summand1,rest1) = parseProd l in
    case rest1 of
      PLUS:rest2 -> let (summand2,rest3) = parseExpr rest2
                    in  (Plus summand1 summand2, rest3)
      (_)        -> (summand1,rest1)


parseProd   :: [Token] -> (Expr, [Token])
parseProd l
    | (factor1, TIMES:rest1) <- pfl = let (factor2, rest2) = parseProd rest1
                                      in  (Times factor1 factor2, rest2)
    | otherwise                     = pfl
  where pfl = parseFactor l

parseFactor :: [Token] -> (Expr, [Token])
parseFactor ((CONST i) : xs) = ((Const i), xs)
parseFactor (LPAREN    : xs) = let (expr, RPAREN : rest ) = parseExpr xs
                               in  (expr, rest)
parseFactor              xs  = error $ "Factor expected, but found: " ++ show xs


test :: Expr -- query GHCI for "test" to test your solution
test = read "1 * (2 + 3 * 4) + 5 * 6 + 7 + 8 * 9"
