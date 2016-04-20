module Heron where

import Data.List (foldl')
import Data.Ratio ((%))

data Rat = Rat {num :: Integer, den :: Integer}


instance Num Rat where
  (Rat a b) + (Rat c d) = Rat (a*d + c*b) (b*d)
  (Rat a b) - (Rat c d) = Rat (a*d - c*b) (b*d)
  (Rat a b) * (Rat c d) = Rat (a * b) (c * d)
  negate (Rat a b)      = Rat (-a) b
  abs    (Rat a b)      | a < 0     = Rat (-a) (b)
                        | otherwise = Rat a b
  signum (Rat a b)      | a < 0     = (-1)
                        | a > 0     = 1
                        | otherwise = 0
  fromInteger a         = Rat a 1

instance Eq Rat where
  (Rat a b) == (Rat c d) = a' == c' && b' == d'
    where (a', b') = denomiate a b
          (c', d') = denomiate c d

instance Ord Rat where
  compare (Rat a b) (Rat c d) = compare (fI a / fI b) (fI c / fI d)
    where fI = fromIntegral

instance Fractional Rat where
  (Rat a b) / (Rat c d) = Rat (a * d) (b * c)
  recip (Rat a b)       = Rat b a
  fromRational = undefined

instance Show Rat where
  show (Rat a b) = '(':show a' ++ " / " ++ show b' ++ ") or " ++ show (((fromIntegral a) :: Double) / ((fromIntegral b) :: Double))
    where (a', b') = denomiate a b

denomiate :: Integer -> Integer -> (Integer, Integer)
denomiate a b = (a `div` d, b `div` d)
  where d = gcd a b

fromDouble :: Double -> Rat
fromDouble number
    | '.' `notElem` numberS = Rat { num = round number, den = 1              }
    | otherwise             = Rat { num = x `div` d   , den = faktor `div` d }
  where numberS = show number
        faktor  = 10 ^ ((length $ dropWhile (/= '.') numberS) - 1)
        x       = round (number * fromIntegral faktor) :: Integer
        d       = gcd x faktor



(//) :: Integer -> Integer -> Rat
a // b = Rat a b

infixr 9 //

heronRat :: Integer -- square root this number
         -> Int     -- how many recursive calls
         -> Rat
heronRat x y = foldl' (\xs x -> acc x xs) (fromInteger 1) (replicate y (fromInteger x))
  where acc :: Rat -> Rat -> Rat
        acc x y_0 = (y_0 + x / y_0) / (fromInteger 2)



heronDouble :: Double  -- square root this number
            -> Int     -- how many recursive calls
            -> Double
heronDouble x y = foldl' (\xs x -> helper x xs) 1 (replicate y x)
  where helper :: Double -> Double -> Double
        helper  x y_0 = (y_0 + x / y_0) / 2

main :: IO ()
main = do
  let r1 = heronRat 9 1
      r2 = heronRat 9 2
      r3 = heronRat 9 3
      r4 = heronRat 9 4
      r5 = heronRat 9 5
      d1 = heronDouble 9 1
      d2 = heronDouble 9 2
      d3 = heronDouble 9 3
      d4 = heronDouble 9 4
      d5 = heronDouble 9 5
  putStrLn "Rational with 1 to 5 recursive calls"
  mapM_ (putStrLn . show) [r1,r2,r3,r4,r5]
  putStrLn "Double with 1 to 5 recursive calls"
  mapM_ (putStrLn . show) [d1,d2,d3,d4,d5]

  putStrLn "Rational sqrt 2 with 10 recursive calls"
  putStrLn (show (heronRat 2 10))

  putStrLn "Double sqrt 2 with 100 recursive calls"
  putStrLn (show (heronDouble 2 100))


