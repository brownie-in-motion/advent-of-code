import Advent
import Control.Applicative ( (<**>) )
import Control.Arrow ( first )
import Control.Monad ( join )
import Data.Bifunctor ( bimap )
import Data.List ( find, foldl', nub, transpose )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Map.Strict ( (!?), Map )
import Data.Sequence ( Seq )
import Data.Set ( Set )

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Direction = L | R | U | D deriving Show

type Location = (Int, Int)
type Adjacency = Map Location [Direction]
type Visited = Set Location

type Row = [Char]
type Board = [Row]

height :: Char -> Int
height 'S' = height 'a'
height 'E' = height 'z'
height x = fromEnum x

reachable :: Char -> Char -> Bool
reachable a b = (height b - height a) < 2

move :: Direction -> Location -> Location
move L (a, b) = (a, b - 1)
move R (a, b) = (a, b + 1)
move U (a, b) = (a - 1, b)
move D (a, b) = (a + 1, b)

-- add a direction to the adjacency list
putD :: Adjacency -> Location -> Direction -> Adjacency
putD a l d = M.alter (return . (d:) . fromMaybe []) l a

-- get directions from a location, with default []
getD :: Adjacency -> Location -> [Direction]
getD a = fromMaybe [] . (a !?)

-- make edges for a row, going right
rightEdges :: Direction -> Adjacency -> [(Int, Int)] -> Row -> Adjacency
rightEdges d m l g = foldl' (\ a -> flip (putD a) d . fst) m $ valid g
    where
        valid = filter (uncurry reachable . snd) . pairs
        pairs = zip l . (tail <**> zip)

-- make edges for board, along the rows in one direction
rowEdges :: Direction -> [([(Int, Int)], [Char])] -> Adjacency -> Adjacency
rowEdges d a m = foldl' (\ a (r, x) -> rightEdges d a r x) m a

back :: [([b], [a])] -> [([b], [a])]
back = map (unzip . reverse . uncurry zip)

locations :: [[(Int, Int)]]
locations = map ((`map` [0..]) . (,)) [0..]

-- make edges for a given board
boardEdges :: [[Char]] -> Adjacency
boardEdges board = foldl' (flip ($)) M.empty $ map ($ board) maps
    where
        rows = zip locations
        columns = zip (transpose locations) . transpose
        lines = zip [L, R, U, D] [back . rows, rows, back . columns, columns]
        maps = map (uncurry (.) . first rowEdges) lines

-- number of steps to the end
-- take visited locations and starting points
--
-- now replaced with `flood` and `steps` favoring fold over recursion
-- as a consequence, `steps` no longer needs an initial visited set
--
-- steps :: Location -> Adjacency -> Visited -> [Location] -> Int
-- steps t a v s
--     | t `elem` s = 0
--     | otherwise = 1 + steps t a (foldl' (flip S.insert) v next) unvisited
--         where
--             unvisited = nub $ filter (not . (`S.member` v)) next
--             next = s >>= uncurry map . bimap (flip move) (getD a) . join (,)

-- bfs outwards one step
flood :: Adjacency -> (Visited, [Location]) -> (Visited, [Location])
flood a (v, s) = (foldl' (flip S.insert) v next, unvisited)
    where
        unvisited = nub $ filter (not . (`S.member` v)) next
        next = s >>= uncurry map . bimap (flip move) (getD a) . join (,)

steps :: Location -> Adjacency -> [Location] -> Int
steps t a s = fst $ fromJust $ find (S.member t . fst . snd) numbered
    where numbered = zip [0..] (iterate (flood a) (S.empty, s))

-- get locations of a given character
locate :: Char -> Board -> [Location]
locate c b = map fst $ filter ((== c) . snd) (zip locations b >>= uncurry zip)

-- from a list of starting characters, get the distance to the end
distanceFrom :: [Char] -> Problem -> Int
distanceFrom s b = steps target adjacency start
    where
        target = head $ locate 'E' b
        adjacency = boardEdges b
        start = s >>= flip locate b

one :: Board -> Int
one = distanceFrom ['S']

two :: Board -> Int
two = distanceFrom ['a', 'S']

main = run lines [show . one, show . two] "../inputs/day-12"
