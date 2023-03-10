import Advent
import Control.Monad.State.Strict ( evalState, get, gets, modify, put, State )
import Data.Bifunctor ( bimap )
import Data.IntMap.Strict ( IntMap )
import Data.List ( foldl' )
import Data.Map.Strict ( Map )
import Data.Maybe ( fromJust )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

type Reference = Int
type Size = Int

type NodeReferences = Map String Reference


-- helper functions for dealing with Node

data Node = Directory String [(String, Node)] | File String Size deriving Show
data NodeM = DirectoryM String NodeReferences | FileM String Size deriving Show

name :: NodeM -> String
name (DirectoryM s _) = s
name (FileM s _) = s

references :: NodeM -> NodeReferences
references (DirectoryM _ r) = r
references (FileM _ _) = error "files cannot contain other files"

newDirectory :: String -> NodeM
newDirectory s = DirectoryM s Map.empty

newFile :: String -> Int -> NodeM
newFile = FileM


-- helper functions for dealing with Tree

type Tree = Node
type TreeM = IntMap NodeM

-- given a reference and name, add it to the node's children
addReference :: Reference -> String -> NodeM -> NodeM
addReference i n (DirectoryM s m) = DirectoryM s (Map.insert n i m)
addReference _ _ _ = error "file cannot contain nodes"

-- take a parent reference, child reference, child node, and tree; add node
addNode :: Reference -> Reference -> NodeM -> TreeM -> TreeM
addNode p c n t = IntMap.adjust (addReference c $ name n) p inserted
    where inserted = IntMap.insert c n t


-- helper functions for dealing with TreeState

-- holds tree, current directory, and next id
type TreeState = (TreeM, [Reference], Reference)

-- the root has special meaning in this problem; the tree must contain one
newT :: TreeState
newT = (IntMap.fromList [(0, DirectoryM "/" Map.empty)], [0], 1)

-- get the tree
getTreeT :: State TreeState TreeM
getTreeT = gets (\ (t, _, _) -> t)

-- get the current directory references
getReferencesT :: State TreeState NodeReferences
getReferencesT = gets unwrap
    where unwrap (t, d, _) = references $ fromJust $ IntMap.lookup (head d) t

-- go up a directory
ascendT :: State TreeState ()
ascendT = modify (\ (t, d, c) -> (t, tail d, c))

-- go to the root
rootT :: State TreeState ()
rootT = modify (\ (t, _, c) -> (t, [0], c))

-- go down into a directory; assume that the directory exists
descendT :: String -> State TreeState ()
descendT name = do
    (tree, dir, curr) <- get
    references <- getReferencesT
    put (tree, fromJust (Map.lookup name references) : dir, curr)

-- insert a node into the tree
insertT :: NodeM -> State TreeState ()
insertT n = modify (\ (t, d, c) -> (addNode (head d) c n t, d, c + 1))

-- check if a directory or file already exists
hasT :: String -> State TreeState Bool
hasT s = Map.member s <$> getReferencesT

-- insert only if nonexistent
insertSafeT :: NodeM -> State TreeState ()
insertSafeT n = hasT (name n) >>= (\ b -> if b then pure () else insertT n)

-- descend, inserting if nonexistent
descendSafeT :: String -> State TreeState()
descendSafeT = uncurry (>>) . bimap addDirectory descendT . (id >>= (,))

-- extract subtree TreeM at index into Tree
extractT :: Int -> State TreeState Tree
extractT i = do
    (tree, _, _) <- get
    case fromJust $ IntMap.lookup i tree of
        DirectoryM s m -> Directory s . zip keys <$> values
            where
                children = Map.toList m
                keys = map fst children
                values = mapM (extractT . snd) children
        FileM s n -> pure $ File s n

-- api

addFile :: String -> Int -> State TreeState()
addFile s = insertSafeT . newFile s

addDirectory :: String -> State TreeState ()
addDirectory = insertSafeT . newDirectory

doVisit :: String -> State TreeState ()
doVisit "/" = rootT
doVisit ".." = ascendT
doVisit s = descendSafeT s

getTree :: State TreeState Tree
getTree = extractT 0


-- parsing

operation :: [String] -> State TreeState ()
operation ["$", "cd", d] = doVisit d
operation ["$", "ls"] = pure ()
operation ["dir", d] = addDirectory d
operation [n, f] = addFile f (read n)
operation _ = error "invalid line"

parse :: String -> Tree
parse s = evalState (mapM_ (operation . split ' ') (lines s) >> getTree) newT


-- solution

foldTree' :: (a -> Int -> a) -> ([a] -> a) -> a -> Tree -> (Int, a)
foldTree' _ _ a (File s n) = (n, a)
foldTree' f m a (Directory _ c) = (size, f acc size)
    where (size, acc) = bimap sum m $ unzip $ map (foldTree' f m a . snd) c

foldTree :: (a -> Int -> a) -> ([a] -> a) -> a -> Tree -> a
foldTree f m a t = snd $ foldTree' f m a t

one :: Tree -> Int
one = foldTree (\ a s -> a + if s > 100000 then 0 else s) sum 0

two :: Tree -> Int
two tree = foldTree best (foldl' best 0) 0 tree
    where
        best a b
            | a >= size && b >= size = min a b
            | otherwise = max a b
        size = flip (-) 40000000 $ foldTree (const id) sum 0 tree


-- boilerplate

main = run parse [show . one, show . two] "../inputs/day-07"
