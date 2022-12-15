import Advent
import Data.Bifunctor ( second )
import Data.Char ( isDigit )
import Data.List ( findIndices, sort )
import Data.Monoid ( (<>) )
import Data.Ord ( compare, Ordering )
import Data.Tuple ( swap )
import Debug.Trace ( traceShowId )

data Packet = V Int | L [Packet] deriving Show

instance Ord Packet where
    compare (V a) (V b) = compare a b
    compare (L []) (L []) = EQ
    compare (L []) (L _) = LT
    compare (L _) (L []) = GT
    compare (L (a:as)) (L (b:bs)) = compare a b <> compare (L as) (L bs)
    compare a@(V _) b@(L _) = compare (L [a]) b
    compare a@(L _) b@(V _) = compare a (L [b])

-- is this bad form?
instance Eq Packet where (==) a b = compare a b == EQ

type Problem = [(Packet, Packet)]

-- don't look at this :)
decode :: String -> (String, Packet)
decode [] = error "empty packet"
decode s@(x:xs)
    | x == '[' = (tail $ fst $ last list, L $ map snd $ tail list)
    | x == ',' = decode xs
    | isDigit x = second (V . read) $ swap (span isDigit s)
    | otherwise = error "invalid packet"
        where
            list = takeUntil ((== ']') . head . fst) next
            next = iterate (decode . fst) (xs, undefined)

parse :: String -> Problem
parse s = map (\ [a, b] -> both packet (a, b)) $ split "" $ lines s
    where packet s = case decode s of
            (_:_, _) -> error "found garbage after packet"
            (_, p) -> p

-- the problem wants everything shifted by one and i am lazy
indices :: (a -> Bool) -> [a] -> [Int]
indices f = map (+ 1) . findIndices f

one :: Problem -> Int
one = sum . indices id . map ((/= GT) . uncurry compare)

two :: Problem -> Int
two = product . indices (`elem` d) . sort . (d ++) . (>>= (\ (a, b) -> [a, b]))
    where d = map (L . pure . L . pure . V) [2, 6]

main = run parse [show . one, show . two] "../inputs/day-13"
