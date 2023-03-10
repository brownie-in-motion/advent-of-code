import Advent
import Control.Monad ( join )
import Data.Bifunctor ( bimap )
import Data.List ( tails, transpose )

parse :: String -> [[Int]]
parse = map (map $ read . (:[])) . lines

bothWays :: ([a] -> [b]) -> (b -> b -> c) -> [a] -> [c]
bothWays f m xs = zipWith m (f xs) (reverse $ f $ reverse xs)

solution
    :: ([b] -> b) -- merge results along column
    -> ([a] -> b) -- merge intermediate computation along row
    -> (a -> a -> a) -- merge results of normal and transpose
    -> ([b] -> [a]) -- create intermediate computation in one direction
    -> [[b]] -- input array
    -> b -- output
solution c r o f trees = c $ zipWith count (standard trees) (flipped trees)
    where
        count = curry $ r . uncurry (zipWith o)
        standard = map (bothWays f o)
        flipped = transpose . standard. transpose

one :: [[Int]] -> Int
one = solution sum (length . filter id) (||) f
    -- takes a list of tree heights and gives a list of booleans
    -- each boolean represents whether a tree is visible from the head
    where f = ((-1 :) . scanl1 max) >>= zipWith (<)

two :: [[Int]] -> Int
two = solution maximum maximum (*) (map (length . seen) . init . tails)
    -- takes a list of tree heights and shows all visible trees from each tree
    -- only does so in one direction (down the list)
    where seen = uncurry takeUntil . bimap ((<=) . head) tail . join (,)

main = run parse [show . one, show . two] "../inputs/day-08"
