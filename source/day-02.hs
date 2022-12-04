import Advent
import Control.Arrow ( (&&&), second )
import Data.Bifunctor ( bimap )

type Problem = [(Int, Int)]

parse :: String -> Problem
parse s = map (decode . second tail . break (== ' ')) $ lines s
    where decode = bimap (offset 'A') (offset 'X') . both head

-- (opponent, move) -> score
score :: (Int, Int) -> Int
score (a, b) = 3 * mod (b - a + 1) 3 + b + 1

-- (opponent, target) -> move
outcome :: (Int, Int) -> Int
outcome (a, b) = mod (a + b - 1) 3

one :: Problem -> Int
one = sum . map score

two :: Problem -> Int
two = sum . map (score . (fst &&& outcome))

solutions :: [Problem -> String]
solutions = [show . one, show . two]

main = run parse solutions "../inputs/day-02"
