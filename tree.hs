import Data.List
import Data.List.Split
import System.IO


data Tree a = EmptyNode
            | Node a [Tree a] 
            deriving (Eq, Show)

printtree :: Tree String -> String
printtree EmptyNode = "()"
printtree (Node a []) = "(" ++ a ++")"
printtree (Node a trees) = "(" ++ a ++ "->" ++ "[" ++ concat [(printtree tree)| tree <- trees] ++ "]"++ ")"
emptytree = EmptyNode

rootT :: Tree String -> String
rootT EmptyNode = error "Empty"
rootT (Node a _) = a



testTree :: Tree String
testTree = Node "/" [Node "etc" [], Node "mnt" [], Node "home" []]

fileSystem :: Tree String
fileSystem = testTree

pwd = putStrLn $ rootT fileSystem

cd :: String -> Tree String 
cd "" = error "Empty path"
cd a = cdUtil (splitOn "/" a) (Just fileSystem)

cdUtil :: [String] ->  (Maybe (Tree String)) -> Tree String
cdUtil _ Nothing = error "Invalid path"
cdUtil [] (Just fs) = fs
cdUtil (x:xs) (Just (Node a trees)) = cdUtil xs (find (\ tree -> (rootT tree) == x) trees)

getNodeTree :: Tree a -> [Tree a]
getNodeTree EmptyNode = error "Empty"
getNodeTree (Node _ trees) = trees


--ls = putStr (foldl (\s a -> s ++ a ++ " ") "" (getAllLst (snd currentDir)))

--testTree = [5 [4 emptyTree emptyTree] [6 emptyTree emptyTree]]