import Advent
import Control.Monad ( liftM2, replicateM_, when )
import Control.Monad.State ( evalState, gets, modify, State )
import Data.Bifunctor ( second )
import Data.Map.Strict ( (!), Map )
import qualified Data.Map.Strict as M

data Dir = L | R deriving (Show, Eq)

pick :: Dir -> (a, a) -> a
pick L = fst
pick R = snd

type Ref = Int

data Node a = Node {
    value :: a,
    prev :: Ref,
    next :: Ref
} deriving Show

data List a = List {
    top :: Ref,
    size :: Int,
    back :: Map Ref (Node a)
} deriving Show

-- linked list helpers

fromList :: [a] -> List a
fromList x = List 0 count $ M.fromList nodes
    where
        nodes = zip (tail prevs) $ zipWith3 Node x prevs (tail $ tail prevs)
        prevs = map (`mod` count) $ cycle $ zipWith const [-1..] x
        count = length x

getNode :: Ref -> State (List a) (Node a)
getNode r = gets ((! r) . back)

getsNode :: (Node a -> b) -> Ref -> State (List a) b
getsNode f = fmap f . getNode

modifyNode :: (Node a -> Node a) -> Ref -> State (List a) ()
modifyNode f r = modify (\ l -> l { back = M.adjust f r $ back l })

modifySize :: (Int -> Int) -> State (List a) ()
modifySize f = modify (\ l -> l { size = f $ size l })

putHead :: Ref -> List a -> List a
putHead r l = l { top = r }

putPrev :: Ref -> Node a -> Node a
putPrev r n = n { prev = r }

putNext :: Ref -> Node a -> Node a
putNext r n = n { next = r }

putRefs :: (Ref, Ref) -> Node a -> Node a
putRefs (a, b) n = n { prev = a, next = b }

-- does not consider ANY edge cases
-- i really hope that it
-- a) isn't already removed
-- b) the list has like... at least a couple members
-- the removed node retains its forward and back pointers
-- originally i made it become a self loop
-- but this is more convenient for later lol
remove :: Ref -> State (List a) ()
remove i = do
    a <- getsNode prev i
    b <- getsNode next i
    modifyNode (putNext b) a
    modifyNode (putPrev a) b
    modifySize (subtract 1)
    gets top >>= flip when (modify $ putHead b) . (== i)

-- special case for if we do nothing
replace :: Ref -> State (List a) ()
replace i = back >> front >> modifySize (+ 1)
    where
        back = getsNode prev i >>= modifyNode (putNext i)
        front = getsNode next i >>= modifyNode (putPrev i)

-- only makes sense if the added vertex was removed
-- same as remove, just don't mess up lmao
-- something something memory management
add :: Dir -> Ref -> Ref -> State (List a) ()
add d i j = if i == j then replace i else do
    (x, y) <- case d of
        L -> uncurry (liftM2 (,)) (getsNode prev j, return j)
        R -> uncurry (liftM2 (,)) (return j, getsNode next j)
    modifyNode (putNext i) x
    modifyNode (putPrev x) i
    modifyNode (putNext y) i
    modifyNode (putPrev i) y
    modifySize (+ 1)

step :: (Node a -> Ref) -> Ref -> Int -> State (List a) Ref
step f r n = iterate (>>= getsNode f) (pure r) !! n

shift :: Ref -> Int -> State (List a) ()
shift r i = remove r >> gets size >>= step f r . (abs i `mod`) >>= add d r
    where (d, f) = if i < 0 then (L, prev) else (R, next)

getList :: State (List a) [a]
getList = gets top >>= getListAfter
    where getListAfter r = do
            t <- gets top
            n <- getsNode next r
            v <- getsNode value r
            if n == t
                then return [v]
                else (v :) <$> getListAfter n

decode :: State (List Int) ()
decode = gets (map (second value) . M.assocs . back) >>= mapM_ (uncurry shift)

type Problem = [Int]

parse :: String -> Problem
parse = map read . lines

answer :: [Int] -> Int
answer x = sum . extract [1000, 2000, 3000] $ dropWhile (/= 0) $ cycle x

solve :: Int -> [Int] -> Int
solve n = answer . evalState (replicateM_ n decode >> getList) . fromList

one :: Problem -> Int
one = solve 1

two :: Problem -> Int
two = solve 10 . map (* 811589153)

main = run parse [show . one, show . two] "../inputs/day-20"
