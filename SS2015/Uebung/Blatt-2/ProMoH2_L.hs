module Main where

-- H2-1

-- | Gesucht: Eine Funktion die eine Liste durchgeht und das Minimum
--   sowie das Maxmimum in einem Tupel ausgibt
--   Bonus: Die Liste darf nur einmal durchgegangen werden

seekMaxMin :: [Double] -> (Double, Double)
seekMaxMin []      = (0,0)
seekMaxMin (l:ls)  = seekMaxMinAcc ls (l, l)
  where seekMaxMinAcc :: [Double] -> (Double, Double) -> (Double, Double)
        seekMaxMinAcc []     (min, max) = (min, max)
        seekMaxMinAcc (x:xs) (min, max) = seekMaxMinAcc xs (newMin, newMax)
            where newMin, newMax :: Double
                  newMin = if x < min then x else min
                  newMax = if x > max then x else max

seekMaxMin' :: [Double] -> (Double, Double)
seekMaxMin' []     = (0,0)
seekMaxMin' [l]    = (l,l)
seekMaxMin' (l:ls) = (min l newMin, max l newMax)
  where (newMin, newMax) = seekMaxMin' ls


-- H2-2

-- a)

type Position = Int
type Move = (Position, Position)
type Towers = ([Int], [Int], [Int])
type Disc = Int

hanoi :: Int -> Position -> Position -> [Move]
hanoi 1      from to = [(from, to)]
hanoi height from to = hanoi newHeight from otherT ++ [(from, to)] ++ hanoi newHeight otherT to
  where otherT = 1+2+3-from-to
        newHeight = height - 1


move :: ([Move], Towers) -> ([Move], Towers)
move ([]       , towers) = ([]  ,           towers)
move (move:rest, towers) = (rest, step move towers)

step :: Move -> Towers -> Towers
step (1,2) (x:xs,   ys,   zs) = (        xs, place x ys,         zs)
step (1,3) (x:xs,   ys,   zs) = (        xs,         ys, place x zs)
step (2,1) (  xs, y:ys,   zs) = (place y xs,         ys,         zs)
step (2,3) (  xs, y:ys,   zs) = (        xs,         ys, place y zs)
step (3,1) (  xs,   ys, z:zs) = (place z xs,         ys,         zs)
step (3,2) (  xs,   ys, z:zs) = (        xs, place z ys,         zs)
step   _         _        = error "Ungültiger Zug!"

place :: Disc -> [Disc] -> [Disc]
place disc []        = [disc]
place disc tower@(upperdisc:_)
  | disc < upperdisc = disc:tower
  | otherwise        = error "Scheibe ist zu groß!"


-- b)

game :: ([Move], Towers) -> Towers
game ([], towers) = towers
game situation    = game (move situation)