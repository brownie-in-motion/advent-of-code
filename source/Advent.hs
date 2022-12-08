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
