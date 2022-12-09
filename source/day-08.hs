import Advent
import Control.Monad ( join )
import Data.Bifunctor ( bimap )
import Data.List ( tails, transpose )

parse :: String -> [[Int]]
parse = map (map $ read . (:[])) . lines

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [] = []
takeUntil f (a:as)
    | f a = [a]
    | otherwise = a : takeUntil f as

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
    where f = ((-1 :) . scanl1 max) >>= zipWith (<)

two :: [[Int]] -> Int
two = solution maximum maximum (*) (map (length . seen) . init . tails)
    where seen = uncurry takeUntil . bimap ((<=) . head) tail . join (,)

main = run parse [show . one, show . two] "../inputs/day-08"
