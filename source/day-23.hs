{-# LANGUAGE TupleSections #-}

import Advent
import Control.Applicative ( liftA2 )
import Control.Monad ( mapM, join )
import Control.Monad.State ( evalState, gets, modify, State )
import Data.Bifunctor ( bimap, first, second )
import Data.List ( foldl1', foldl', transpose )
import Data.Map ( Map )
import Data.Set ( Set )
import Data.Tuple ( swap )

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace ( traceShowId )

type Vector = (Int, Int)

operate :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
operate f (a, b) (c, d) = (f a c, f b d)

entries :: (a -> b) -> (a, a) -> (b, b)
entries f (a, b)= (f a, f b)

adjacent :: [Vector]
adjacent = filter (uncurry (||) . both (/= 0)) $ join (liftA2 (,)) [-1, 0, 1]

moved :: [Vector] -> Vector -> [Vector]
moved xs x = map (operate (+) x) xs

type Board = Set Vector

parse :: String -> Board
parse = S.fromList . map fst . filter ((== '#') . snd) . enumTwo . lines

-- board and direction list
type Game = (Board, [Vector])

initial :: Board -> Game
initial = (, cycle [(-1, 0), (1, 0), (0, -1), (0, 1)])

getsB :: (Board -> a) -> State Game a
getsB f = gets (f . fst)

movers :: State Game (Set Vector)
movers = gets ((\ b -> S.filter (any (`S.member` b) . moved adjacent) b) . fst)

-- map of position, first parent, and number of occurrences
type Counter = Map Vector (Vector, Int)

update :: Counter -> (Vector, Vector) -> Counter
update c (u, v)
    | M.member v c = M.adjust (second (+ 1)) v c
    | otherwise = M.insert v (u, 1) c

unique :: [(Vector, Vector)] -> [(Vector, Vector)]
unique vs = map swap . M.toList . M.map fst $ M.filter ((== 1) . snd) counts
    where counts = foldl' update M.empty vs

area :: Board -> Vector -> (Vector, [Vector])
area b = (,) <*> (flip (.) (+) . side >>= (`mapM` [-1, 0, 1]))
    where
        side (_, 0) = second
        side (0, _) = first
        side _ = undefined

dropInvalid :: Board -> [Vector] -> Vector -> [Vector]
dropInvalid b o v = (`moved` v) $ map fst $ dropWhile invalid directions
    where
        invalid = any (`S.member` b) . (`moved` v) . snd
        directions = map (area b) o

propose :: Set Vector -> State Game [(Vector, Vector)]
propose m = gets (head . transpose . uncurry labeled . second (take 4))
    where
        labeled b o = zipWith (map . (,)) <*> works b o $ S.toList m
        works b o l = map (dropInvalid b o) l

complete :: [(Vector, Vector)] -> State Game ()
complete vs = modify (first move)
    where move = foldl1' (.) . map (uncurry (.) . bimap S.delete S.insert) $ vs

move :: Set Vector -> State Game ()
move m = propose m >>= complete . unique >> modify (second tail)

step :: State Game ()
step = movers >>= move

rounds :: State Game Int
rounds = do
    elves <- movers
    if S.size elves == 0
        then return 1
        else (+ 1) <$> (move elves >> rounds)

rectangle :: State Game Int
rectangle = do
    board <- gets (S.toList . fst)
    let ((a, b), (c, d)) = both (($ board) . foldl1 . operate) (min, max)
    return $ (c - a + 1) * (d - b + 1) - length board

one :: Board -> Int
one = evalState (execute >> rectangle) . initial
    where execute = iterate (>> step) (pure undefined) !! 10

two :: Board -> Int
two = evalState rounds . initial

main = run parse [show . one, show . two] "../inputs/day-23"
