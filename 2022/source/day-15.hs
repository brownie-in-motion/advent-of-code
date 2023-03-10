import Advent
import Control.Arrow ( (&&&) )
import Data.Bifunctor ( second )
import Data.Char ( isDigit )
import Data.List ( sort )
import Data.Maybe ( mapMaybe )

type Point = (Int, Int)

type Problem = [(Point, Point)]

unflatten :: [a] -> ((a, a), (a, a))
unflatten [a, b, c, d] = ((a, b), (c, d))
unflatten _ = error "invalid input"

num :: Char -> Bool
num = uncurry (||) . (isDigit &&& (== '-'))

parse :: String -> Problem
parse = map (unflatten . map value . extract [2, 3, 8, 9] . split ' ') . lines
    where value = read . takeWhile num . dropWhile (not . num)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (a, b) (c, d) = abs (a - c) + abs (b - d)

-- given y value, center, and radius (manhattan)
-- compute the left and right endpoints of intersection
segment :: Int -> Point -> Int -> Maybe (Int, Int)
segment y (a, b) r
    | remaining < 0 = Nothing
    | otherwise = Just (a - remaining, a + remaining)
        where remaining = r - abs (b - y)

segments :: Int -> Problem -> [(Int, Int)]
segments y = mapMaybe (uncurry (flip $ (<*>) (segment y) . distance))

-- given an interval and a sorted list of intervals, eat elements off the head
combine :: (Int, Int) -> [(Int, Int)] -> ((Int, Int), [(Int, Int)])
combine a [] = (a, [])
combine (a, b) l@((x, y):xs)
    | b < x = ((a, b), l)
    | otherwise = combine (a, max b y) xs

-- merge intervals as far as they will go
union :: [(Int, Int)] -> [(Int, Int)]
union [] = []
union [x] = [x]
union (x:xs) = uncurry (:) $ second union $ combine x xs

-- divide sorted intervals on a sorted list of positions
divide :: [Int] -> [(Int, Int)] -> [(Int, Int)]
divide [] [] = []
divide a@(x:xs) b@(t@(u, v):ys)
    | x < u = divide xs b
    | u <= x && x <= v = valid [(u, x - 1), (x + 1, v)] ++ divide xs ys
    | v < x = t : divide a ys
        where valid = filter (uncurry (<=))
divide _ b = b

regions :: Int -> Problem -> [(Int, Int)]
regions = curry (union . sort . uncurry segments)

-- given a row and a list of sensor values, return a list of intervals
excluded :: Int -> Problem -> [(Int, Int)]
excluded i p = divide exclude $ regions i p
    where exclude = sort . map fst . filter ((== i) . snd) $ map snd p

-- given a list of intervals, invert it to a list of gaps
gaps :: [(Int, Int)] -> [(Int, Int)]
gaps s = zip (map ((+) 1 . snd) $ init s) (map (subtract 1 . fst) $ tail s)

-- limit a list of intervals to a given range
ranged :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
ranged (a, b) = mapMaybe squish
    where squish (x, y)
            | y < a || b < x = Nothing
            | otherwise = Just (max a x, min b y)

-- given a y value, find a possible beacon location
search :: Problem -> Int -> Maybe Point
search p i = case ranged (0, 4000000) $ gaps $ regions i p of
    [] -> Nothing
    (x:xs) -> Just (i, fst x)

one :: Problem -> Int
one = sum . map ((+ 1) . uncurry (flip (-))) . excluded 2000000

two :: Problem -> Int
two p = uncurry (+) $ second (* 4000000) location
    where location = head $ mapMaybe (search p) [0 .. 4000000]

main = run parse [show . one, show . two] "../inputs/day-15"
