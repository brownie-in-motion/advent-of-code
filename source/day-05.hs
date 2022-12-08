{-# LANGUAGE RankNTypes #-}

import Advent

import Control.Monad ( foldM, forM_, replicateM_ )
import Control.Monad.ST ( runST, ST )
import Data.Array.ST ( getElems, newListArray, readArray, writeArray, STArray )
import Data.Functor ( ($>) )
import Data.List ( transpose )

-- parsing

type Stack = [Char]
type Boxes = [Stack]
type Instruction = (Int, Int, Int)
type Instructions = [Instruction]
type Problem = (Boxes, Instructions)

parseBoxes :: [String] -> Boxes
parseBoxes = map (dropWhile (== ' ')) . transpose . map (map (!! 1) . chunks 4)

parseInstructions :: [String] -> Instructions
parseInstructions = map (parseTokens . split ' ')
    where parseTokens = \ [_, a, _, b, _, c] -> (read a, read b, read c)

parse :: String -> Problem
parse s = (parseBoxes $ init front, parseInstructions back)
    where [front, back] = split "" $ lines s


-- helper functions for array

type BoxArray s = STArray s Int Stack

putBoxArray :: Boxes -> ST s (STArray s Int Stack)
putBoxArray = ((,) 1 . length) >>= newListArray

getsS :: (Stack -> a) -> BoxArray s -> Int -> ST s a
getsS f arr i = f <$> readArray arr i

modifyS :: (Stack -> Stack) -> BoxArray s -> Int -> ST s ()
modifyS f arr i = readArray arr i >>= writeArray arr i . f

getsModifyS :: (Stack -> a) -> (Stack -> Stack) -> BoxArray s -> Int -> ST s a
getsModifyS f g arr i = getsS f arr i >>= pass (const $ modifyS g arr i)

-- move one element from stack i to stack j
moveStack :: BoxArray s -> Int -> Int -> ST s ()
moveStack arr i j = popStack arr i >>= pushStack arr j
    where
        pushStack arr i c = modifyS (c :) arr i
        popStack = getsModifyS head tail

-- move n elements from stack i to stack j
shiftStack :: BoxArray s -> Int -> Int -> Int -> ST s ()
shiftStack arr n i j = takeStack arr n i >>= appendStack arr j
    where
        takeStack arr n = getsModifyS (take n) (drop n) arr
        appendStack arr i s = modifyS (s ++) arr i

-- get the top elements
peekStacks :: BoxArray s -> ST s Stack
peekStacks arr = map head <$> getElems arr


-- solutions

type Evaluator s = BoxArray s -> Instruction -> ST s ()

-- instruction evaluation strategy for part one
evalOne :: Evaluator s
evalOne arr (n, i, j) = replicateM_ n (moveStack arr i j)

-- instruction evaluation strategy for part two
evalTwo :: Evaluator s
evalTwo arr (n, i, j) = shiftStack arr n i j

solution :: (forall s. Evaluator s) -> Problem -> [Char]
solution f (b, i) = runST (putBoxArray b >>= pass (forM_ i . f) >>= peekStacks)


-- boilerplate

main = run parse [solution evalOne, solution evalTwo] "../inputs/day-05"
