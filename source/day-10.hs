import Advent
import Data.List ( foldl', intercalate, scanl' )

data Instruction = ADDX Int | NOOP
type Program = [Instruction]

parse :: String -> Program
parse = map (parseLine . split ' ') . lines
    where
        parseLine ["addx", n] = ADDX $ read n
        parseLine ["noop"] = NOOP
        parseLine _ = error "invalid instruction"

deltas :: Instruction -> [Int]
deltas (ADDX n) = [0, n]
deltas NOOP = [0]

values :: Program -> [Int]
values = init . scanl' (+) 1 . (>>= deltas)

-- extract :: [Int] -> [a] -> [a]
-- extract = helper 1
--     where
--         helper _ [] _ = []
--         helper n indices@(i:is) (x:xs)
--             | i == n = x : helper (n + 1) is xs
--             | otherwise = helper (n + 1) indices xs
--         helper _ _ _ = error "illegal input"
-- 
-- one :: Program -> Int
-- one = let i = [20, 60..220] in sum . zipWith (*) i . extract i . values

addProduct :: Int -> (Int, Int) -> Int
addProduct = (. uncurry (*)) . (+)

one :: Program -> Int
one = foldl' addProduct 0 . filter (keepIndex . fst) . zip [1..220] . values
    where keepIndex = (== 0) . (`mod` 40) . subtract 20

two :: Program -> String
two = intercalate "\n" . map (zipWith draw [0..40]) . chunks 40 . values
    where draw i x = if abs (i - x) < 2 then '#' else '.'

main = run parse [show . one, two] "../inputs/day-10"
