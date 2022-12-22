import Data.List
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


getNodeTree :: Tree a -> [Tree a]
getNodeTree EmptyNode = error "Empty"
getNodeTree (Node _ trees) = trees


--ls = putStr (foldl (\s a -> s ++ a ++ " ") "" (getAllLst (snd currentDir)))

--testTree = [5 [4 emptyTree emptyTree] [6 emptyTree emptyTree]]