import Advent
import Control.Arrow ( (&&&) )
import Control.Monad ( replicateM_, liftM2, when )
import Control.Monad.State ( evalState, gets, modify, State )
import Data.Bifunctor ( bimap, first )
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace ( traceShowId )

data Direction = L | R | D
type Problem = [Direction]

type Vector = (Int, Int)

-- map (jet, rock) -> height
type Log = Map (Int, Int) [Int]

type Board = Set Vector
type Delta = Vector -> Vector
type Rock = ([Vector], Int)
type Program = (Board, Int, Vector, [(Delta, Int)], [(Rock, Int)], Log)

decode :: Char -> Direction
decode '<' = L
decode '>' = R
decode _ = error "cannot parse input"

delta :: Direction -> Vector -> Vector
delta L (a, b) = (a, b - 1)
delta R (a, b) = (a, b + 1)
delta D (a, b) = (a - 1, b)

divide :: Int -> Int -> (Int, Int)
divide a = div a &&& mod a

rocks :: [Rock]
rocks = map ((,) <*> liftM2 (-) maximum minimum . map fst) [a, b, c, d, e]
    where
        a = [(0, 0), (0, 1), (0, 2), (0, 3)]
        b = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
        c = [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
        d = [(0, 0), (1, 0), (2, 0), (3, 0)]
        e = [(0, 0), (0, 1), (1, 0), (1, 1)]

enum :: [a] -> [(a, Int)]
enum = flip zip [0..]

begin :: Problem -> Program
begin m = (S.empty, 0, (3, 2), numberedDeltas, numberedRocks, M.empty)
    where
        numberedDeltas = cycle $ enum $ map delta m
        numberedRocks = cycle $ enum rocks

getBoard :: State Program Board
getBoard = gets (\ (b, _, _, _, _, _) -> b)

getCell :: Vector -> State Program Bool
getCell p = gets (\ (b, _, _, _, _, _) -> border p || S.member p b)
    where border (x, y) = x < 0 || y < 0 || y > 6

putCell :: Vector -> State Program ()
putCell p = modify (\ (b, h, c, d, r, l) -> (S.insert p b, h, c, d, r, l))

getRock :: State Program Rock
getRock = gets (\ (_, _, (x, y), _, r, _) -> first (shift x y) $ fst $ head r)
    where shift x y = map $ bimap (+ x) (+ y)

state :: [(Delta, Int)] -> [(Rock, Int)] -> (Int, Int)
state d r = (snd (head r), snd (head d))

nextRockUpdate :: Program -> Program
nextRockUpdate (b, h, (x, _), d, r, l) = (b, top, (top + 4, 2), d, tail r, log)
    where
        log = M.insert key (top : M.findWithDefault [] key l) l
        key = state d r
        top = max h (x + snd (fst (head r)))

nextRock :: State Program ()
nextRock = modify nextRockUpdate

getHeights :: (Int, Int) -> State Program [Int]
getHeights s = gets (\ (_, _, _, _, _, l) -> M.findWithDefault [] s l)

getState :: State Program (Int, Int)
getState = gets (\ (_, _, _, d, r, l) -> state d r)

getMove :: State Program Delta
getMove = gets (\ (_, _, _, d, _, _) -> fst $ head d)

nextMove :: State Program ()
nextMove = modify (\ (b, h, c, d, r, l) -> (b, h, c, tail d, r, l))

canMove :: Delta -> State Program Bool
canMove move = not . or <$> (getRock >>= mapM (getCell . move) . fst)

doMove :: Delta -> State Program ()
doMove move = modify (\ (b, h, c, d, r, l) -> (b, h, move c, d, r, l))

writeRock :: State Program ()
writeRock = getRock >>= mapM_ putCell . fst

tryMove :: State Program ()
tryMove = getMove >>= liftM2 (>>=) canMove (flip when . doMove) >> nextMove

dropRock :: State Program (Int, Int)
dropRock = tryMove >> do
    fall <- canMove (delta D)
    state <- getState
    if fall
        then doMove (delta D) >> dropRock
        else writeRock >> nextRock >> return state

dropRocks :: Int -> State Program ()
dropRocks n = replicateM_ n dropRock

getTop :: State Program Int
getTop = gets (\ (_, h, _, _, _, _) -> h + 1)

dropUntilDouble :: State Program (Int, (Int, Int))
dropUntilDouble = do
    state <- dropRock
    heights <- getHeights state
    if fst state == 0 && length heights == 2
        then return (0, state)
        else first (+ 1) <$> dropUntilDouble

heightsUntilState :: (Int, Int) -> State Program [Int]
heightsUntilState s = do
    height <- getTop
    state <- dropRock
    if state == s
        then return [height]
        else (height :) <$> heightsUntilState s

-- get the
-- a) initial rock count
-- b) initial height
-- c) cycle height deltas
collectData :: State Program (Int, Int, [Int])
collectData = do
    (rocks, state) <- dropUntilDouble
    heights <- heightsUntilState state
    height <- head . tail <$> getHeights state
    return (rocks, height, map (subtract height) heights)

parse :: String -> Problem
parse = map decode . takeWhile (/= '\n')

one :: Problem -> Int
one = evalState (dropRocks 2022 >> getTop) . begin

two p = initialHeight + cycleHeight + remainderHeight
    where
        remainderHeight = (0 : deltas) !! remainder
        cycleHeight = last deltas * cycles
        (cycles, remainder) = divide (1000000000000 - offset) $ length deltas
        (offset, initialHeight, deltas) = evalState collectData $ begin p

main = run parse [show . one, show . two] "../inputs/day-17"
