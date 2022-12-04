import Advent
import Data.List ( sort )

type Problem = [Int]

parse :: String -> [Int]
parse s = map (sum . map read) (split "" $ lines s)

one :: Problem -> Int
one = foldr max 0

two :: Int -> Problem -> Int
two n = sum . take n . reverse . sort

solutions :: [Problem -> String]
solutions = [show . one, show . two 3]

main = run parse solutions "../inputs/day-01"
