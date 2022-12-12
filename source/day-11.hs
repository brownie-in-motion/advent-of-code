import Advent
import Control.Monad ( join, replicateM, zipWithM )
import Control.Monad.ST ( runST, ST )
import Data.Array.ST ( newListArray, readArray, writeArray, STArray )
import Data.Char ( isDigit )
import Data.List ( sort, transpose )
import Data.Sequence ( (|>), Seq )
import qualified Data.Sequence as S

type Reference = Int

type Queue = Seq Int
type Items s = STArray s Reference Queue

putA :: [[Int]] -> ST s (Items s)
putA = ((,) 0 . subtract 1 . length >>= newListArray) . map S.fromList

modifyA :: (Queue -> Queue) -> Items s -> Reference -> ST s ()
modifyA f arr i = readArray arr i >>= writeArray arr i . f

getsA :: (Queue -> a) -> Items s -> Reference -> ST s a
getsA f arr i = f <$> readArray arr i

-- offer to the end of a given monkey's queue
offerA :: Items s -> Reference -> Int -> ST s ()
offerA arr i e = modifyA (|> e) arr i

-- empty a monkey's queue
emptyA :: Items s -> Reference -> ST s Queue
emptyA arr i = readArray arr i >>= pass (const $ writeArray arr i S.empty)

-- operation, test, (true target, false target)
type Behavior = (Int -> Int, Int, (Reference, Reference))
type Behaviors = [Behavior]

type Problem = ([[Int]], Behaviors)

decode :: [Char] -> (Int -> Int -> Int)
decode "+" = (+)
decode "*" = (*)
decode _ = error "unknown operator"

parseI :: String -> [Int]
parseI line = map (read . takeWhile isDigit) nums
    where
        ("Starting":"items:":nums) = split ' ' line

parseB :: [String] -> Behavior
parseB lines = (operation a, test b, (true c, false d))
    where
        [a, b, c, d] = map (split ' ') lines

        operation ["Operation:", _, _, _, o, "old"] = join $ decode o
        operation ["Operation:", _, _, _, o, n] = decode o $ read n
        operation _ = error "could not parse operation"

        test ["Test:", _, _, n] = read n
        test _ = error "could not parse test"

        true ["If", "true:", _, _, _, n] = read n
        true _ = error "could not parse true target"

        false ["If", "false:", _, _, _, n] = read n
        false _ = error "could not parse false target"

parse :: String -> Problem
parse = unzip . map (parseM . map (dropWhile (== ' '))) . split "" . lines
    where
        parseM (_:items:behavior) = (parseI items, parseB behavior)
        parseM _ = error "could not parse monkey"

-- given a behavior and a monkey, give the number inspected
process :: (Int -> Int) -> Items s -> Behavior -> Reference -> ST s Int
process f arr (o, t, n) r = S.length <$> (worries >>= pass move)
    where
        -- ST s Int of new worry levels
        worries = ((f . o) <$>) <$> emptyA arr r
        -- ST s () after giving the items to the new monkeys
        move = mapM_ ((flip when n . (== 0) . (`mod` t)) >>= offerA arr)

-- take a strategy for worry reduction
-- perform a round, returning the number inspected by each monkey
turn :: (Int -> Int) -> Items s -> Behaviors -> ST s [Int]
turn f arr bs = zipWithM (process f arr) bs [0..]

-- take a strategy for worry reduction
-- perform n rounds and give the resulting visits
rounds :: (Int -> Int) -> Problem -> Int -> Int
rounds f (i, b) k = answer $ runST (putA i >>= replicateM k . flip (turn f) b)
    where answer = product . take 2 . reverse . sort . map sum . transpose

-- after each round, divide by three as in the problem
one :: Problem -> Int
one = flip (rounds (`div` 3)) 20

-- after each round, ideally modulo by the lmc, but product works too :p
two :: Problem -> Int
two (i, b) = rounds (`mod` modulus) (i, b) 10000
    where modulus = product $ map (\ (_, n, _) -> n) b

main = run parse [show . one, show . two] "../inputs/day-11"
