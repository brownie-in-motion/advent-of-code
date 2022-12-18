module Advent where

import Control.Monad ( void )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor ( ($>) )

-- both :: (a -> b) -> (a, a) -> (b, b)
-- both f (a, b) = (f a, f b)

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

chunks :: Int -> [a] -> [[a]]
chunks a x = case splitAt a x of
    ([], []) -> []
    (chunk, []) -> [chunk]
    (chunk, rest) -> chunk : chunks a rest

extract :: [Int] -> [a] -> [a]
extract = helper 0
    where
        helper _ [] _ = []
        helper n indices@(i:is) (x:xs)
            | i == n = x : helper (n + 1) is xs
            | otherwise = helper (n + 1) indices xs
        helper _ _ _ = error "illegal input"


fan :: Functor f => f (a -> b) -> a -> f b
fan f x = fmap ($ x) f

halve :: [a] -> ([a], [a])
halve a = splitAt (div (length a) 2) a

offset :: Char -> Char -> Int
offset a b = fromEnum b - fromEnum a

pass :: Functor f => (a -> f b) -> a -> f a
pass f a = f a $> a

split :: Eq a => a -> [a] -> [[a]]
split s a = case break (==s) a of
    (begin, []) -> [begin]
    (begin, end) -> begin : split s (tail end)

run :: (String -> a) -> [a -> String] -> String -> IO ()
run p s f = readFile f >>= void . sequence . fan (map (putStrLn .) s) . p

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [] = []
takeUntil f (a:as)
    | f a = [a]
    | otherwise = a : takeUntil f as

which :: Bool -> (a, a) -> a
which True = fst
which False = snd
