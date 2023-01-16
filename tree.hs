import Data.List
import Data.List.Split
import Data.Optional
import Data.String
import System.IO


data Tree a = EmptyNode
            | Node (Tree a) a [Tree a] 
            | File a a
            deriving (Eq, Show)
            

printtree :: Tree String -> String
printtree EmptyNode = "()"
printtree (Node _ a []) = "(" ++ a ++")"
printtree (Node _ a trees) = "(" ++ a ++ "->" ++ "[" ++ concat [(printtree tree)| tree <- trees] ++ "]"++ ")"
emptytree = EmptyNode

getParrentNode :: Tree String -> Tree String
getParrentNode (EmptyNode) = error "Empty"
getParrentNode (Node p _ _) = p
getParrentNode (File _ _ ) = error "This is a file"

rootT :: Tree String -> String
rootT EmptyNode = error "Empty"
rootT (Node _ a _) = a
rootT (File name _) = name

getNodeFromPath :: String -> Tree String -> Tree String
getNodeFromPath "" fs = fs
getNodeFromPath str fs = cdUtil (splitOn "/" str) (Just fs)

testTree :: Tree String
testTree = Node EmptyNode "/" [Node testTree "etc" [ (File "bativan.txt" "Bai Ivan"), (File "eqk.txt" " e mnogo qk")], Node testTree "mnt" [], Node testTree "home" []]

fileSystem :: Tree String
fileSystem = testTree

pwd = putStrLn $ rootT fileSystem

cd :: String -> Tree String 
cd "" = error "Empty path"
cd a = cdUtil (splitOn "/" a) (Just fileSystem)

getChildren :: Tree String -> [String]
getChildren (Node _ _ []) = []
getChildren (Node _ _ trees) = map rootT trees 

makeStringFromChildren :: [String] -> String
makeStringFromChildren ([]) = ""
makeStringFromChildren (x:xs ) = x ++ " " ++ makeStringFromChildren xs  


ls :: Maybe String -> Tree String -> String 
ls Nothing fs = makeStringFromChildren (getChildren fs)
ls (Just str) fs = makeStringFromChildren (getChildren (getNodeFromPath str fs))

cdUtil :: [String] ->  (Maybe (Tree String)) -> Tree String
cdUtil _ Nothing = testTree
cdUtil [] (Just fs) = fs
cdUtil _ (Just (File s txt)) = File s txt 
cdUtil (x:xs) (Just (Node par a trees))
 | (x == "") = cdUtil xs (Just fileSystem)
 | (x == "..") = cdUtil xs (Just par)
 | otherwise  = cdUtil xs (find (\ tree -> (rootT tree) == x) trees)

getNodeTree :: Tree a -> [Tree a]
getNodeTree EmptyNode = error "Empty"
getNodeTree (Node _ _ trees) = trees


--ls = putStr (foldl (\s a -> s ++ a ++ " ") "" (getAllLst (snd currentDir)))

--testTree = [5 [4 emptyTree emptyTree] [6 emptyTree emptyTree]]

isFile :: Tree String -> Bool
isFile (File _ _ ) = True
isFile _ = False

checkFileName :: Tree String -> String -> Bool
checkFileName (File name _ ) n = (n == name)

hasChildWithName :: [Tree String] -> String -> Bool
hasChildWithName [] _ = False
hasChildWithName children name = (filter (\ f -> (isFile f) /= True || (checkFileName f name) == True ) children) /= []

addToParrent :: Tree String -> Tree String -> Tree String
addToParrent EmptyNode _ = EmptyNode
addToParrent (Node par name chidren) file = (Node (addToParrent par file) name (file : chidren))


removeFromParrent :: Tree String -> Tree String -> Tree String
removeFromParrent EmptyNode _  = EmptyNode
removeFromParrent (Node p2 nodeN children) callTree = (Node (removeFromParrent p2 (Node p2 nodeN (callTree : filter (\n -> (rootT n) /= (rootT callTree)) children))) nodeN (callTree : filter (\n -> (rootT n) /= (rootT callTree)) children))

rmcmd :: [String] -> Tree String -> Tree String
rmcmd [] fs = fs 
rmcmd (x:xs) (Node par name children) = (Node (removeFromParrent par (Node par name (filter (\ f -> (isFile f) /= True || (checkFileName f x) /= True ) children)))  name (filter (\ f -> (isFile f) /= True || (checkFileName f x) /= True ) children))

getFileContent :: Tree String -> String
getFileContent (File _ cont) = cont

catCmd :: [String] -> Tree String -> String 
catCmd [] _ = ""
catCmd (x:xs) fs = (getFileContent (getNodeFromPath x fs)) ++ (catCmd xs fs)

catCmd2 :: [String] -> String -> Tree String -> Tree String
catCmd2 li fileName (Node par name children) = (Node par name ((File fileName (catCmd li (Node par name children))) : children))

getHead :: [String] -> String
getHead (x:xs) = x

getAfterSign :: [String] -> String
getAfterSign [] = ""
getAfterSign (x:xs) = if x == ">" then getHead xs else getAfterSign xs

getBeforeSign :: [String] -> [String]
getBeforeSign (">":_) = []
getBeforeSign (x:xs) = x : (getBeforeSign xs)

catCmdProcess :: [String] -> Tree String -> IO()
catCmdProcess li fs = if getAfterSign li == [] then (putStrLn (catCmd li fs)) else (inputCommand (catCmd2 (getBeforeSign li) (getAfterSign li) fs))

--rmcmd2 :: Tree String -> Tree String
--rmcmd2 EmptyNode -> EmptyNode
--rmcmd2 (Node par str li) -> (node par str li)
--rmcmd2 (File name text par)  

processCommand _ [] = error "Empty command"
processCommand fs (x:xs) = do
    case x of "pwd" -> putStrLn $ rootT fs
              "cd" -> inputCommand $ getNodeFromPath (head xs) fs
              "ls" -> if xs == [] then putStrLn (ls Nothing fs) else putStrLn (ls (Just (head xs)) fs)
              "rm" -> inputCommand $ rmcmd xs fs  
              "cat" -> catCmdProcess xs fs 
              otherwise -> putStrLn "Unknown command"
    inputCommand fs

--inputCommand fs

inputCommand fs = do
     cmd <- getLine
     let args = splitOn " " cmd
     processCommand fs args
main = do
    inputCommand testTree
    --let args = splitOn " " cmd 
