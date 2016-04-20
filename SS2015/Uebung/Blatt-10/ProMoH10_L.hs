module Main where

import System.IO

-- | H10-2

-- a)

frage :: String -> IO String
frage question = do
  putStrLn question   -- IO ()
  getLine             -- IO String

-- analog würde auch diese Definition gehen (wenn 'return' erlaubt wäre):

-- putStrLn question
-- answer <- getLine
-- return answer


-- b)

infixr 1 <=<

-- DO - Notation

(<=<) :: Monad m => (b ->  m c) -> (a -> m b) -> a -> m c
f <=< g = \x -> do
   y <- g x
   f y


-- c)

frage2 :: String -> IO String
frage2 question = ((\_ -> getLine) <=< putStrLn) question

-- point-free
-- frage2 = (\_ -> getLine) <=< putStrLn
-- putStrLn :: String -> IO ()
-- (\_ -> getLine)  :: a -> IO String

-- d)

-- BIND - Notation

komp2 :: Monad m => (b ->  m c) -> (a -> m b) -> a -> m c
komp2 f g x = g x >>= \y -> f y

-- point-free
-- komp2 f g = \x -> g x >>= f