import Advent
import Data.List ( foldl1', intersect )

type Problem = [String]

common :: Ord a => [[a]] -> a
common = head . foldl1' intersect

commonTwo :: Ord a => [a] -> [a] -> a
commonTwo a b = common [a, b]

score :: Char -> Int
score a
    | 'a' <= a && a <= 'z' = offset 'a' a + 1
    | 'A' <= a && a <= 'Z' = offset 'A' a + 27
    | otherwise = error "bad input"

one :: Problem -> Int
one = sum . map (score . uncurry commonTwo . halve)

two :: Problem -> Int
two = sum . map (score . common) . chunks 3

solutions :: [Problem -> String]
solutions = [show . one, show . two]

main = run lines solutions "../inputs/day-03"
