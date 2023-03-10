import Advent
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( first, second )
import Data.List ( foldl' )
import Data.Map ( Map )
import Data.Set ( Set )

import qualified Data.Map as M
import qualified Data.Set as S

type Vector = (Int, Int)

operate :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
operate f (a, b) (c, d) = (f a c, f b d)

entries :: (a -> b) -> (a, a) -> (b, b)
entries f (a, b)= (f a, f b)

data Direction = L | R | U | D deriving Show

type Blizzards = Map Vector [Direction]

data Problem = Problem {
    start :: Vector,
    end :: Vector,
    bounds :: Vector,
    blizzards :: Blizzards
} deriving Show

decode :: Char -> Direction
decode '<' = L
decode '>' = R
decode '^' = U
decode 'v' = D
decode x = error "unknown symbol"

delta :: Direction -> Vector
delta L = (0, -1)
delta R = (0, 1)
delta U = (-1, 0)
delta D = (1, 0)

parse :: String -> Problem
parse s = Problem {
        start = head blanks,
        end = last blanks,
        bounds = both (maximum . flip map labeled . (. fst)) (fst, snd),
        blizzards = M.fromList $ map (second (pure . decode)) snow
    } where
        snow = filter (liftA2 (&&) (/= '#') (/= '.') . snd) labeled
        blanks = map fst $ filter ((== '.') . snd) labeled
        labeled = enumTwo $ lines s

reset :: Vector -> Direction -> Vector -> Vector
reset (_, w) L (a, _) = (a, w - 1)
reset (_, _) R (a, _) = (a, 0 + 1)
reset (h, _) U (_, b) = (h - 1, b)
reset (_, _) D (_, b) = (0 + 1, b)

inside :: Vector -> Vector -> Bool
inside (h, w) (x, y) = x > 0 && y > 0 && x < h && y < w

-- given bounds, a direction, and a position, find the next position
move :: Vector -> Direction -> Vector -> Vector
move b d v = if inside b next then next else reset b d next
    where next@(x, y) = operate (+) v $ delta d

moveAll :: Vector -> [Direction] -> Vector -> [(Vector, Direction)]
moveAll b ds v = (map (flip (move b) v) >>= zip) ds

insert :: (Vector, Direction) -> Blizzards -> Blizzards
insert (v, d) = ((d :) . M.findWithDefault [] v) >>= M.insert v

-- given bounds and starting configuration, give a list of all future ones
states :: Vector -> Blizzards -> [Blizzards]
states b m = ((:) <*> states b) (foldl' (.) id (map insert moved) M.empty)
    where moved = concatMap (uncurry $ flip (moveAll b)) $ M.toList m

allowed :: Problem -> Blizzards -> Vector -> Bool
allowed p m v = (bounded || opening) && unoccupied
    where
        bounded = inside (bounds p) v
        opening = (v == start p) || (v == end p)
        unoccupied = not (v `M.member` m)

children :: Problem -> Blizzards -> Vector -> [Vector]
children p m v = filter (allowed p m) neighbors
    where neighbors = map (operate (+) v) $ (0, 0) : map delta [L, R, U, D]

reachable :: Problem -> (Set Vector, [Blizzards]) -> (Set Vector, [Blizzards])
reachable p (c, m:n) = (S.fromList $ concatMap (children p m) $ S.toList c, n)
reachable _ _ = error "oops ran out of snow"

find :: (a -> Bool) -> [a] -> (Int, a)
find f (x:xs) = if f x then (0, x) else first (+ 1) $ find f xs
find f _ = error "empty list"

runUntil :: Vector -> Vector -> Problem -> [Blizzards] -> (Int, [Blizzards])
runUntil s e p b = second snd $ find (S.member e . fst) states
    where states = iterate (reachable p) (S.singleton s, b)

one :: Problem -> Int
one p = fst $ runUntil (start p) (end p) p (states (bounds p) $ blizzards p)

two :: Problem -> Int
two p = a + b + c
    where
        x = states (bounds p) $ blizzards p
        (a, y) = runUntil (start p) (end p) p x
        (b, z) = runUntil (end p) (start p) p y
        (c, _) = runUntil (start p) (end p) p z

main = run parse [show . one, show . two] "../inputs/day-24"
