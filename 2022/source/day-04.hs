import Advent
import Data.Tuple ( swap )

type Instance = ((Int, Int), (Int, Int))
type Problem = [Instance]

parse :: String -> Problem
parse = map (tuples . map (map read . split '-') . split ',') . lines
    where
        tuples [[a, b], [c, d]] = ((a, b), (c, d))
        tuples _ = error "bad input"

count :: (Bool -> Bool -> Bool) -> (Instance -> Bool) -> Problem -> Int
count c f = length . filter ((\ f t -> f t `c` f (swap t)) f)

one :: Problem -> Int
one = count (||) (\ ((a, b), (c, d)) -> a <= c && b >= d)

two :: Problem -> Int
two = count (&&) (\ ((a, b), (c, d)) -> not $ a < c && b < c)

solutions :: [Problem -> String]
solutions = [show . one, show . two]

main = run parse solutions "../inputs/day-04"
