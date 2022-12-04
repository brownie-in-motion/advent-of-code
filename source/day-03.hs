import Advent
import Data.List ( foldl1', sort, intersect )

type Problem = [String]

common :: Ord a => [[a]] -> a
common = head . foldl1' intersect

-- common :: Ord a => [[a]] -> a
-- common x
--     | all (== small) heads = small
--     | otherwise = common $ map (remove (== small)) x
--     where
--         remove f l@(x:xs)
--             | f x = xs
--             | otherwise = l
--         remove _ _ = error "empty list"
--         heads = map head x
--         small = minimum heads

commonTwo :: Ord a => [a] -> [a] -> a
commonTwo a b = common [a, b]

-- commonTwo :: Ord a => [a] -> [a] -> a
-- commonTwo a@(x:xs) b@(y:ys)
--     | x == y = x
--     | x < y = commonTwo xs b
--     | x > y = commonTwo a ys
-- commonTwo _ _ = error "bad input"

score :: Char -> Int
score a
    | 'a' <= a && a <= 'z' = offset 'a' a + 1
    | 'A' <= a && a <= 'Z' = offset 'A' a + 27
    | otherwise = error "bad input"

one :: Problem -> Int
one = sum . map (score . uncurry commonTwo . both sort . halve)

two :: Problem -> Int
two = sum . map (score . common) . chunks 3 . map sort

solutions :: [Problem -> String]
solutions = [show . one, show . two]

main = run lines solutions "../inputs/day-03"
