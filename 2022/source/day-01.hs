import Advent
import Data.List ( sort )

type Problem = [[Int]]

parse :: String -> Problem
parse = map (map read) . split "" . lines

one :: Problem -> Int
one = maximum . map sum

two :: Int -> Problem -> Int
two n = sum . take n . reverse . sort . map sum

solutions :: [Problem -> String]
solutions = [show . one, show . two 3]

main = run parse solutions "../inputs/day-01"
