import Advent
import Control.Monad ( liftM2 )
import Data.Bifunctor ( bimap, first )
import Data.Char ( isAlpha, isDigit )
import Data.List ( foldl1', foldl' )
import Data.Map ( (!), Map )
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.Map as M

-- random useful vector math stuff
type Vector = (Int, Int)

operate :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
operate f (a, b) (c, d) = (f a c, f b d)

entries :: (a -> b) -> (a, a) -> (b, b)
entries f (a, b)= (f a, f b)

-- things to deal with directions
data Turn = CW | CC deriving Show
data Direction = L | R | U | D deriving ( Ord, Eq, Show )

delta :: Direction -> Vector
delta L = (0, -1)
delta R = (0, 1)
delta U = (-1, 0)
delta D = (1, 0)

directions :: [Direction]
directions = [L, R, U, D]

spin :: Int -> Direction -> Direction
spin = flip $ (!!) . iterate clock
    where
        clock L = U
        clock R = D
        clock U = R
        clock D = L

turn :: Turn -> Direction -> Direction
turn CW = spin 1
turn CC = spin 3

facing :: Direction -> Int
facing R = 0
facing D = 1
facing L = 2
facing U = 3

-- shift a position to a given edge
edge :: Int -> Direction -> Vector -> Vector
edge _ L (x, _) = (x, 0)
edge w R (x, _) = (x, w - 1)
edge _ U (_, y) = (0, y)
edge w D (_, y) = (w - 1, y)

-- garbage to deal with connecting up the net
data TState = C | A Direction | O deriving Show

transition :: TState -> Direction -> TState
transition C n = A . spin 2 $ n
transition (A d) n
    | d == n = C
    | d == spin 2 n = O
    | otherwise = A d
transition O n = A n

type Face = Vector
type Net = [Face]

-- these get updated in parallel
type FaceIn = Map Face Direction
type FaceOut = Map Direction Face

-- graph with all the faces
type Faces = Map Face (FaceIn, FaceOut)

-- insert d a b does two things:
-- 1. add a -> d to b's facein
-- 2. add d -> a to b's faceout
insert :: Direction -> Face -> Face -> Faces -> Faces
insert d a = M.adjust (bimap (M.insert a d) (M.insert d a))

add :: Face -> (TState, Face) -> Faces -> Faces
add a (A d, b) = insert d a b
add _ _ = id

-- add edges of other faces involving the current face
-- ie suppose the net looks like this:
--
-- .#..
-- ####
-- .#..
--
-- then, edges on (0, 1) should yield a function that adds incoming and
-- outgoing edges from and to (0, 1) on the faces that it touches and those
-- faces only. it that is, it only changes the faces:
-- (1, 0), (1, 1), (1, 2), (3, 1)
edgesF :: Set Face -> Face -> Faces -> Faces
edgesF n f = edgesH n (S.singleton f) f C f

-- helper function for the above
edgesH :: Set Face -> Set Face -> Face -> TState -> Face -> Faces -> Faces
edgesH n v i s f = added . children
    where
        visit = liftM2 (&&) (`S.member` n) (not . (`S.member` v))
        children = foldl' (.) id $ map (uncurry (edgesH n visited i)) states
        visited = foldl' (flip S.insert) v $ map snd new
        added = foldl' (.) id $ map (add i) states
        states = map (first (transition s)) new
        new = filter (visit . snd) next
        next = map ((,) <*> operate (+) f . delta) directions

-- build the faces map
edges :: Net -> Faces
edges n = foldl' (.) id (map (edgesF $ S.fromList n) n) initial
    where initial = M.fromList . zip n $ repeat (M.empty, M.empty)


-- we store the obstacles for each face
-- coordinates are relative to the face's top left
-- this makes part two easier
type Board = Map Face (Set Vector)
data Instruction = Move Int | Turn Turn deriving Show

decode :: String -> Turn
decode "L" = CC
decode "R" = CW
decode _ = error "unknown direction"

-- convert face coordinates to world coordinates
real :: Int -> Face -> Vector -> Vector
real w f = operate (+) (entries (* w) f)

-- convert world coordinates to face coordinates
face :: Int -> Vector -> (Face, Vector)
face w v = (entries (`div` w) v, entries (`mod` w) v)

-- add an obstacle to the board; if the face doesn't exist, add it
addObstacle :: Face -> Vector -> Board -> Board
addObstacle f v b = M.insert f (S.insert v $ M.findWithDefault S.empty f b) b

-- ((tile width, board), instructions)
type Problem = ((Int, Board), [Instruction])

parseB :: [String] -> (Int, Board)
parseB i = (tileWidth, board)
    where
        board = foldl' (flip ($)) M.empty $ map (uncurry addObstacle) obstacles
        obstacles = map (face tileWidth . fst) $ filter ((== '#') . snd) path
        tileWidth = floor . sqrt . fromIntegral $ length path `div` 6
        path = filter ((/= ' ') . snd) $ enumTwo i

parseI :: String -> [Instruction]
parseI [] = []
parseI s@(x:xs) = uncurry (:) . bimap instruction parseI . span kind $ s
    where (instruction, kind) = if isDigit x
            then (Move . read, isDigit)
            else (Turn . decode, isAlpha)

parse :: String -> Problem
parse = bimap parseB (parseI . last) . span (/= "") . lines

-- movement state: face, relative position, and direction
type MState = (Face, Vector, Direction)

type Wrapper = Faces -> Int -> MState -> MState

-- wrapping strategy: 2d
flatWrap :: Wrapper
flatWrap n w (f, v, d) = (next, edge w opposite v, d)
    where
        next = if M.member adjacent n then adjacent else target
        adjacent = operate (+) (delta d) f
        target = last $ takeWhile (`M.member` n) wrap
        wrap = iterate (operate (+) (delta opposite)) f
        opposite = spin 2 d

rotate :: Int -> Direction -> Direction -> Vector -> Vector
rotate w a b (x, y) = case (facing b - facing a) `mod` 4 of
    0 -> (x, y)
    1 -> (y, w - x - 1)
    2 -> (w - x - 1, w - y - 1)
    3 -> (w - y - 1, x)
    _ -> error "how did we get here"

-- wrapping strategy: cube
cubeWrap :: Wrapper
cubeWrap m w (face, v, d) = (target, transformed, direction)
    where
        transformed = edge w opposite $ rotate w d direction v
        direction = spin 2 opposite
        opposite = fst (m ! target) ! face
        target = snd (m ! face) ! d

-- check if a location is outside a face
outside :: Int -> Vector -> Bool
outside w = uncurry (||) . entries (liftM2 (||) (< 0) (>= w))

-- perform one step
-- takes a lot of cached stuff for efficiency reasons
-- 1. map Face to (FaceIn, FaceOut) describing how faces connect
-- 2. the size of each face
-- 3. map Face to set of Vector, the board obstacles
-- 4. the function describing how wrapping happens when out of bounds
-- returns a function that transitions from MState to MState
step :: Faces -> Int -> Board -> Wrapper -> MState -> MState
step c w b t s@(f, v, d)
    | S.member v' (b ! f') = s
    | otherwise = (f', v', d')
        where
            (f', v', d') = if outside w next
                then t c w s
                else (f, next, d)
            next = operate (+) v $ delta d

-- helper function for applying a list of functions (a -> a)
apply :: [a -> a] -> a -> a
apply = foldl1' (flip (.))

-- given a wrapping strategy, find the result from the problem
execute :: Wrapper -> Problem -> MState
execute f ((w, b), i) = apply (map next i) (fst $ M.findMin b, (0, 0), R)
    where
        next (Move k) = (!! k) . iterate (step (edges $ M.keys b) w b f)
        next (Turn t) = \ (x, y, z) -> (x, y, turn t z)

-- compute the answer from the final state
answer :: Problem -> MState -> Int
answer ((w, _), _) (f, v, d) = 1000 * (x + 1) + 4 * (y + 1) + facing d
    where (x, y) = real w f v

one :: Problem -> Int
one = answer <*> execute flatWrap

two :: Problem -> Int
two = answer <*> execute cubeWrap

main = run parse [show . one, show . two] "../inputs/day-22"
