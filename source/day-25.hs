import Advent

type Problem = [String]

decode :: Char -> Int
decode '=' = -2
decode '-' = -1
decode '0' = 0
decode '1' = 1
decode '2' = 2
decode _ = error "unknown symbol"

encode :: Int -> Char
encode (-2) = '='
encode (-1) = '-'
encode 0 = '0'
encode 1 = '1'
encode 2 = '2'
encode _ = error "unknown number"

unsnafu :: String -> Int
unsnafu = sum . zipWith (*) (map (5 ^) [0..]) . reverse . map decode

-- convert numbers to base five
convert :: Int -> [Int]
convert = reverse . f
    where
        f 0 = []
        f x = (x `mod` 5) : f (x `div` 5)

-- map of k to 2 * (5 ^ 0 + 5 * 1 + ... + 5 ^ k)
powers :: [Int]
powers = map (2 *) $ iterate ((+ 1) . (* 5)) 1

-- a) find the smallest member of `powers` that is greater than n
-- b) add it to n and convert to base five
-- c) subtract two from each place
snafu :: Int -> String
snafu n = map (encode . subtract 2) $ convert (n + biggest)
    where biggest = head $ dropWhile (< n) powers

one :: Problem -> String
one = snafu . sum . map unsnafu

main = run lines [one] "../inputs/day-25"
