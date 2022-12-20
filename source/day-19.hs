import Advent
import Control.Monad ( liftM2, sequence )
import Data.List ( foldl', zip4 )
import Data.Maybe ( fromJust, isJust )
import Data.Tree ( Tree )

import qualified Data.Tree as T

data Blueprint = Blueprint {
    ore :: Int, -- ore
    clay :: Int, -- ore
    obsidian :: (Int, Int), -- ore, clay
    geode :: (Int, Int) -- ore, obsidian
} deriving Show

unflatten :: [Int] -> Blueprint
unflatten [a, b, c, d, e, f] = Blueprint a b (c, d) (e, f)
unflatten _ = error "incorrect number of values"

decode :: Blueprint -> Vec (Vec Int)
decode bp = (or, cl, ob, ge)
    where
        or = (ore bp, 0, 0, 0)
        cl = (clay bp, 0, 0, 0)
        ob = (fst $ obsidian bp, snd $ obsidian bp, 0, 0)
        ge = (fst $ geode bp, 0, snd $ geode bp, 0)

-- maximum burderns for each resource type
-- useful because we never want more than that many bots
burdens :: Blueprint -> Vec Int
burdens = foldl' (pair max) (0, 0, 0, maxBound) . list . decode

type Problem = [Blueprint]

parse :: String -> Problem
parse = map (unflatten . values) . lines
    where values = map read . extract [6, 12, 18, 21, 27, 30] . split ' '

type Vec a = (a, a, a, a)

pair :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
pair f (a, b, c, d) (w, x, y, z) = (f a w, f b x, f c y, f d z)

each :: (a -> b) -> Vec a -> Vec b
each f (w, x, y, z) = (f w, f x, f y, f z)

at :: Int -> (a -> a) -> Vec a -> Vec a
at 0 f (w, x, y, z) = (f w, x, y, z)
at 1 f (w, x, y, z) = (w, f x, y, z)
at 2 f (w, x, y, z) = (w, x, f y, z)
at 3 f (w, x, y, z) = (w, x, y, f z)
at _ _ _ = error "invalid dimension"

list :: Vec a -> [a]
list (a, b, c, d) = [a, b, c, d]

unlist :: [a] -> Vec a
unlist [a, b, c, d] = (a, b, c, d)
unlist _ = error "bad list"

-- remaining time, [b]ots, [r]esources
type GameState = (Blueprint, Int, Vec Int, Vec Int)

-- time needed given a rate, necessary resources, and current resources
-- we use ceiling division here: up a b = 1 + (a - 1) `div` b
time :: Int -> Int -> Int -> Maybe Int
time b n r
    | b == 0 && n /= 0 = Nothing
    | b == 0 && n == 0 = Just 0
    | otherwise = Just $ max 0 $ 1 + (n - r - 1) `div` b

-- time needed to create a bot
-- rate (bots), necessary resources, and current resources vectors
bot :: Vec Int -> Vec Int -> Vec Int -> Maybe Int
bot b n = fmap maximum . sequence . list . pair ($) (pair time b n)

-- given a blueprint, [b]ots, and [r]esources, give wait times for new bots
wait :: Blueprint -> Vec Int -> Vec Int -> Vec (Maybe Int)
wait bp b r = each (flip (bot b) r) $ decode bp

-- the dumb Vec juggling here is probably really slow
-- ...but it works so we're done with this problem
next :: GameState -> [GameState]
next (bp, t, b, r) = map unwrap succeed
    where
        -- unwrap from maybe
        unwrap (w, x, y, z) = (w, fromJust x, y, each fromJust z)
        -- builds that succeed, only
        succeed = filter (\ (_, t, _, _) -> isJust t) builds
        -- states resulting in builds
        builds = zip4 (repeat bp) (list times) (list bots) (list paid)
        -- the remaining time corresponding to building each type
        times = each (liftM2 (-) $ Just t) waits :: Vec (Maybe Int)
        -- number of bots after building (add one to the relevant bot)
        bots = each ($ b) (each (`at` (+ 1)) (0, 1, 2, 3)) :: Vec (Vec Int)
        -- inventories after waiting, after spending
        paid = pair (pair (liftM2 (-))) inv prices :: Vec (Vec (Maybe Int))
        -- inventories after waiting, before spending
        inv = each (pair (liftM2 (+)) rs) inc :: Vec (Vec (Maybe Int))
        -- inventory increase from bots
        inc = each (`each` bs) waiters :: Vec (Vec (Maybe Int))
        -- functions that simulate waiting the necessary time
        waiters = each (liftM2 (*)) waits :: Vec (Maybe Int -> Maybe Int)
        -- wait times
        waits = each (liftM2 (+) $ Just 1) $ wait bp b r :: Vec (Maybe Int)
        -- prices of the bot types
        prices = each (each Just) $ decode bp :: Vec (Vec (Maybe Int))
        -- wrapped resources
        rs = each Just r
        -- wrapped bots
        bs = each Just b

prune :: GameState -> Bool
prune (bp, t, b, r) = timeout || extra
    where
        timeout = t < 1
        extra = or . list $ pair (>) b $ burdens bp

buildNode :: GameState -> (Int, [GameState])
buildNode s@(_, t, (_, _, _, g'), (_, _, _, g)) = (t * g' + g, children)
    where children = filter (not . prune) $ next s

buildNodeD :: GameState -> ((Int, Vec Int, Vec Int), [GameState])
buildNodeD s@(_, t, b, r) = ((t, b, r), filter (not . prune) $ next s)

tree :: Int -> Blueprint -> Tree Int
tree n b = T.unfoldTree buildNode (b, n, (1, 0, 0, 0), (0, 0, 0, 0))

treeD :: Int -> Blueprint -> Tree (Int, Vec Int, Vec Int)
treeD n b = T.unfoldTree buildNodeD (b, n, (1, 0, 0, 0), (0, 0, 0, 0))

fold :: (Int -> [Int] -> Int)
fold a b = max a $ maximum (0 : b)

one :: Problem -> Int
one = sum . zipWith (*) [1..] . map (T.foldTree fold . tree 24)

two :: Problem -> Int
two = product . map (T.foldTree fold . tree 32) . take 3

main = run parse [show . one, show . two] "../inputs/day-19"
