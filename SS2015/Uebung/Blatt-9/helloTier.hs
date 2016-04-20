module Main where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))

-- unwords :: [String] -> String

--  unwords ["Hello", "world", "!"]
--  => "Hello world !"

main :: IO ()
main = do

-- Das sagt der Konsole, dass jedes Mal wenn eine Aktion kommt,
-- die einen Character ausdruckt, wird sie sofort ausgeführt,
-- anstatt auf manchen Systemen zu warten, bis ein "\n\r" kommt
  hSetBuffering stdout NoBuffering

-- putStrLn :: String -> IO ()
  putStrLn "Hi! Gib bitte zuerst dein Lieblingstier und dann in die nächste Zeile deine Lieblingseigenschaft ein:"

-- getLine :: IO String
--            |_______|
--String        /
-- |           /
-- |          |
  tier <- getLine
  eigenschaft <- getLine

-- 1.
  --putStrLn ("Psst, willst du " ++ eigenschaft ++ ' ' : tier ++ " kaufen?")

-- 2.
  --putStrLn $ "Psst, willst du " ++ eigenschaft ++ ' ' : tier ++ " kaufen?"

-- 3.
  --putStrLn ("Psst, willst du " ++ (unwords [eigenschaft,tier]) ++ " kaufen?")

-- 4.
  let ausgabe = "Psst, willst du " ++ eigenschaft ++ ' ' : tier ++ " kaufen?"
  putStrLn ausgabe

