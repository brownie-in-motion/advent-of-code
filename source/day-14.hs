import Advent
import Control.Applicative ( liftA2 )
import Control.Monad ( (<=<), foldM, when )
import Control.Monad.State ( execState, gets, modify, State )
import Data.Bifunctor ( bimap )
import Data.Set ( Set )
import qualified Data.Set as S

-- this is pretty obnoxious but i dont know how to name things
(=&&=) :: (Monad m) => Bool -> m Bool -> m Bool
(=&&=) a b = if a then b else pure False

(=||=) :: (Monad m) => Bool -> m Bool -> m Bool
(=||=) a b = if a then pure True else b

andM :: (Monad m) => [m Bool] -> m Bool
andM = foldM (=&&=) True

type Point = (Int, Int)
type Path = [Point]
type Problem = [Path]

type Board = Set Point

evens :: [a] -> [a]
evens = map snd . filter ((== 0) . (`mod` 2) . fst) . zip [0..]

tuple :: [a] -> (a, a)
tuple [a, b] = (a, b)
tuple _ = error "incorrect length"

parse :: String -> Problem
parse = map (map (tuple . map read . split ',') . evens . split ' ') . lines

range :: Int -> Int -> [Int]
range a b
    | a <= b = [a .. b]
    | otherwise = [b .. a]

fill :: Point -> Point -> [Point]
fill (a, b) (c, d) = liftA2 (,) (range a c) (range b d)

board :: Problem -> [Point]
board = (>>= (uncurry fill <=< (tail >>= zip)))

below :: Point -> [Point]
below (a, b) = map (bimap (+ a) (+ b)) [(0, 1), (-1, 1), (1, 1)]

-- read this left to right
-- blocked || (in bounds && children are supported)
supported :: (Point -> Bool) -> Int -> Point -> State (Board, Int) Bool
supported c b p = blocked >>= (=||= ((snd p < b) =&&= valid >>= place))
    where
        blocked = c p =||= gets (S.member p . fst)
        valid = andM (map (supported c b) (below p))
        place = pass (`when` modify (bimap (S.insert p) (+ 1)))

count :: (Point -> Bool) -> Int -> [Point] -> Int
count f b w = snd $ execState (supported f b (500, 0)) (S.fromList w, 0)

one :: Problem -> Int
one = (maximum . map snd >>= count (const False)) . board

two :: Problem -> Int
two b = count ((== base + 2) . snd) (base + 2) walls
    where
        base = maximum $ map snd walls
        walls = board b

main = run parse [show . one, show . two] "../inputs/day-14"
