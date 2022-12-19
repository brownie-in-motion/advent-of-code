import Advent
import Control.Monad ( liftM2 )
import Control.Monad.State ( evalState, gets, modify, State )
import Data.Set ( Set )
import qualified Data.Set as S

type Vector = (Int, Int, Int)
type Problem = [Vector]

parse :: String -> Problem
parse = map ((\ [a, b, c] -> (a, b, c)) . map read . split ',') . lines

negative :: Vector -> Vector
negative (x, y, z) = (- x, - y, - z)

zero :: Vector
zero = (0, 0, 0)

big :: Vector
big = (maxBound, maxBound, maxBound)

pair :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
pair f (a, b, c) (x, y, z) = (f a x, f b y, f c z)

each :: (a -> b) -> (a, a, a) -> (b, b, b)
each f (x, y, z) = (f x, f y, f z)

uncurrry :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurrry f (a, b, c) = f a b c

deltas :: [Vector]
deltas = [(0, 0, 1), (0, 1, 0), (1, 0, 0)] >>= ((:) <*> pure . negative)

neighbors :: Vector -> [Vector]
neighbors = flip map deltas . pair (+)

type Grid = Set Vector

covered :: Vector -> Grid -> Int
covered v = length . filter id . flip map (neighbors v) . flip S.member

-- part 1

insertG :: Vector -> State Grid ()
insertG v = modify (S.insert v)

coveredG :: Vector -> State Grid Int
coveredG = gets . covered

one :: Problem -> Int
one p = evalState (liftM2 subtract covered sides) S.empty
    where
        sides = gets ((* 6) . S.size)
        covered = sum <$> mapM (((2 *) <$>) . liftM2 (>>) insertG coveredG) p

-- part 2

type Board = (Grid, Vector, Vector)

insertB :: Vector -> State Board ()
insertB v = modify (\ (g, a, b) -> (S.insert v g, pair min v a, pair max v b))

hasB :: Vector -> State Board Bool
hasB v = gets (\ (g, _, _) -> S.member v g)

coveredB :: Vector -> State Board Int
coveredB v = gets (\ (g, _, _) -> covered v g)

boundsB :: State Board (Vector, Vector)
boundsB = gets (\ (_, a, b) -> (each (subtract 1) a, each (+ 1) b))

gridB :: State Board Grid
gridB = gets (\ (g, _, _) -> g)

contained :: (Vector, Vector) -> Vector -> Bool
contained (a, b) v = every (pair (<=) a v) && every (pair (>=) b v)
    where every (x, y, z) = x && y && z

-- given bounds, starting, exclude, return visited
expandB :: (Vector, Vector) -> Set Vector -> Set Vector -> Set Vector
expandB b s v
    | null s = s
    | otherwise = S.union s $ expandB b next $ S.union s v
        where
            next = S.fromList (filter keep $ S.toList s >>= neighbors)
            keep = liftM2 (&&) (contained b) (not . (`S.member` v))

outerAirB :: State Board (Set Vector)
outerAirB = liftM2 (expandB <*> (S.fromList . pure . fst)) boundsB gridB

two :: Problem -> Int
two p = evalState (mapM_ insertB p >> outerAirB >>= cover) (S.empty, big, zero)
    where cover = (sum <$>) . mapM coveredB . S.toList

main = run parse [show . one, show . two] "../inputs/day-18"
