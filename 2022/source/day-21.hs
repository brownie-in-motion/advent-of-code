import Advent
import Data.Map ( (!), fromList, Map )
import qualified Data.Map as Map

data Operation = A | S | M | D deriving Show
type Value = Int

func :: Operation -> (Int -> Int -> Int)
func A = (+)
func S = (-)
func M = (*)
func D = div

data Expression = Binary {
    operation :: Operation,
    left :: String,
    right :: String
} | Value Value | Variable

type Problem = Map String Expression

parseLine :: [String] -> (String, Expression)
parseLine [a, b] = (init a, Value $ read b)
parseLine [a, b, c, d] = (init a, Binary (decode c) b d)
    where
        decode "+" = A
        decode "-" = S
        decode "*" = M
        decode "/" = D
        decode _ = error "unknown operation"
parseLine _ = error "bad input"

parse :: String -> Problem
parse = fromList . map (parseLine . split ' ') . lines

data Tree = Root Operation (Tree, Tree) | Leaf Value | Unit deriving Show

buildTree :: String -> Problem -> Tree
buildTree s p = case p ! s of
    Binary o l r -> Root o (buildTree l p, buildTree r p)
    Value v -> Leaf v
    Variable -> Unit

evaluate :: Tree -> Int
evaluate (Root o (l, r)) = func o (evaluate l) (evaluate r)
evaluate (Leaf v) = v
evaluate _ = error "cannot evaluate subtree"

-- caching this would be easy but the input isn't that big
-- it would also make Tree less clean, which is a bit frustruating
constant :: Tree -> Bool
constant Unit = False
constant (Leaf v) = True
constant (Root _ (l, r)) = constant l && constant r

-- invert the left side onto the right
solve :: Tree -> Tree -> Tree
solve Unit tree = tree
solve (Root o (left, right)) tree = case (constant left, constant right) of
    (False, False) -> error "too many variables found"
    (False, True) -> solve left $ case o of
        A -> Root S (tree, right)
        S -> Root A (tree, right)
        M -> Root D (tree, right)
        D -> Root M (tree, right)
    (True, False) -> solve right $ case o of
        A -> Root S (tree, left)
        S -> Root S (left, tree)
        M -> Root D (tree, left)
        D -> Root D (left, tree)
    (True, True) -> error "no variables found"
solve (Leaf _) _ = error "how did we get here"

one :: Problem -> Int
one = evaluate . buildTree "root"

-- treat the root as an `=` vertex
-- solve for the single variable
result :: Tree -> Tree
result (Root _ (l, r)) = case (constant l, constant r) of
    (False, True) -> solve l r
    (True, False) -> solve r l
    _ -> error "incorrect number of variables"
result _ = error "no equation"

two :: Problem -> Int
two = evaluate . result . buildTree "root" . Map.insert "humn" Variable

main = run parse [show . one, show . two] "../inputs/day-21"
