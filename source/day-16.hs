{-# LANGUAGE TupleSections #-}

import Advent
import Control.Applicative ( liftA2 )
import Control.Monad ( join )
import Data.Bifunctor ( bimap, second )
import Data.Char ( isAlpha, isDigit )
import Data.List ( foldl', tails )
import Data.Map ( (!), Map )
import Data.Maybe ( fromMaybe )
import Data.Set ( Set )
import Data.Tree ( Tree )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tree as T

type Values = Map String Int
type Adjacency = Map String (Map String Int)
type Problem = (Values, Adjacency)

-- get weight of (String, String) edge from adjacency list
getEdge :: (String, String) -> Adjacency -> Maybe Int
getEdge (x, y) a = M.lookup x a >>= M.lookup y

-- put (String, String) and weight into adjacency list
putEdge :: (String, String) -> Int -> Adjacency -> Adjacency
putEdge (x, y) i = putEdgeLeft (x, y) i . putEdgeLeft (y, x) i
    where putEdgeLeft (x, y) i = M.insertWith M.union x (M.fromList [(y, i)])

-- a min that behaves like max: if only one is Nothing, then take the other
minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing x = x
minMaybe x Nothing = x
minMaybe x y = min x y

parseLine :: [String] -> (String, (Int, [String]))
parseLine (_:n:_:_:r:xs) = (n, (rate, valves))
    where
        rate = read $ takeWhile isDigit $ dropWhile (not . isDigit) r
        valves = map (takeWhile isAlpha) $ drop 4 xs
parseLine _ = error "invalid input"

parse :: String -> Problem
parse s = bimap create (create . map (M.fromList . map (, 1))) (unzip rows)
    where
        create = M.fromList . zip labels
        (labels, rows) = unzip $ map (parseLine . split ' ') $ lines s

-- add self edges
self :: Adjacency -> Adjacency
self a = foldl' (flip (`putEdge` 0)) a $ join zip $ M.keys a

-- floyd warshall update step
update :: Adjacency -> ((String, String), String) -> Adjacency
update d ((i, j), k) = maybe d (flip (putEdge (i, j)) d) lowest
    where
        [o, l, r] = map (`getEdge` d) [(i, j), (i, k), (k, j)]
        lowest = minMaybe o $ liftA2 (+) l r

-- floyd warshall
distances :: Adjacency -> Adjacency
distances a = foldl' update (self a) triplets
    where triplets = (join (liftA2 (,)) >>= liftA2 (,)) $ M.keys a

-- remove zeros but keep the initial position
prune :: String -> Values -> Adjacency -> Adjacency
prune s v a = M.filterWithKey keep $ M.map (M.filterWithKey keep) a
    where keep k _ = k == s || (v ! k) /= 0

-- remaining time, current valve, remaining valves, current flow
type State = (Int, String, Set String, Int)

child :: Adjacency -> Int -> State -> String -> State
child a i (t, c, r, f) n = (t - (a ! c ! n) - 1, n, S.delete n r, i)

-- tree unfold
build :: Values -> Adjacency -> (State -> ((String, Int), [State]))
build v a s@(t, c, r, f) = ((c, flow), map (child a flow s) $ S.toList next)
    where
        flow = t * v ! c + f
        next = S.filter (maybe False (< t) . (`getEdge` a) . (,) c) r

-- make the tree given starting valve, time, and problem
buildTree :: String -> Int -> Problem -> Tree (String, Int)
buildTree s k p@(v, a) = T.unfoldTree (build v $ prune s v $ distances a) start
    where start = (k, s, S.delete s (S.fromList $ M.keys a), 0)

path :: Tree (String, Int) -> [(Int, [String])]
path = let f (s, i) = map (second (s :)) . ((i, []) :) . concat in T.foldTree f

subsetScores :: [(Int, [String])] -> Map (Set String) Int
subsetScores p = foldl' highest M.empty $ map (second S.fromList) p
    where highest m (i, s) = M.insert s (max i (fromMaybe 0 $ M.lookup s m)) m

pairs :: [a] -> [(a, a)]
pairs x = init (init $ tails x) >>= (\ (x:xs) -> map (x,) xs)

one :: Problem -> Int
one = maximum . map snd . last . T.levels . buildTree "AA" 30

two :: Problem -> Int
two p = maximum $ map (uncurry (+) . both (scores !)) combinations
    where
        combinations = filter disjoint $ pairs $ M.keys scores
        disjoint = (== S.singleton "AA") . uncurry S.intersection
        scores = subsetScores $ path (buildTree "AA" 26 p)

main = run parse [show . one, show . two] "../inputs/day-16"
