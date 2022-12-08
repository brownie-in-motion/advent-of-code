import Advent
import Data.List ( nub, tails )

marker :: Int -> String -> Int
marker n = (+ n) . length . takeWhile (nub >>= (/=)) . map (take n) . tails

main = run id [show . marker 4, show . marker 14] "../inputs/day-06"
