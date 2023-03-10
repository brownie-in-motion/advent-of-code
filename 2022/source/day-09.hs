import Advent
import Data.Bifunctor ( bimap )
import Data.List ( foldl', nub, scanl' )
import Data.Tuple ( swap )

data Direction = L | R | U | D

type Problem = [(Direction, Int)]

decode :: String -> Direction
decode "L" = L
decode "R" = R
decode "U" = U
decode "D" = D
decode _ = error "unknown direction"

delta :: Direction -> (Int, Int)
delta L = (-1, 0)
delta R = (1, 0)
delta U = (0, 1)
delta D = (0, -1)

parse :: String -> Problem
parse = map ((\ [d, n] -> (decode d, read n)) . split ' ') . lines

operate :: (Int -> Int -> Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
operate f (a, b) (c, d) = (f a c, f b d)

threshold :: (Int, Int) -> Bool
threshold = uncurry (||) . both ((> 1) . abs)

step :: Int -> Int
step n
    | n == 0 = 0
    | otherwise = (abs >>= div) n

-- tail position -> head position -> new tail position
follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow t h = if threshold $ operate (-) h t then moved else t
    where moved = operate (+) t $ both step $ operate (-) h t

-- problem -> head positions
heads :: Problem -> [(Int, Int)]
heads p = scanl' (operate (+)) (0, 0) (p >>= change)
    where change = uncurry (flip id) . bimap delta replicate

one :: Problem -> Int
one = length . nub . scanl1 follow . heads

two :: Problem -> Int
two = length . nub . (!! 9) . iterate (scanl1 follow) . heads

main = run parse [show . one, show . two] "../inputs/day-09"
