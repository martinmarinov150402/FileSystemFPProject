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
cdUtil _ Nothing = error "Invalid path"
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

rmcmd :: [String] -> Tree String -> Tree String
rmcmd [] fs = fs 
rmcmd (x:xs) (Node par name children) = (Node par name (filter (\ f -> (isFile f) /= True || (checkFileName f x) /= True ) children))

getFileContent :: Tree String -> String
getFileContent (File _ cont) = cont

catCmd :: [String] -> Tree String -> String 
catCmd [] _ = ""
catCmd (x:xs) fs = (getFileContent (getNodeFromPath x fs)) ++ (catCmd xs fs)

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
              "cat" -> putStrLn $ catCmd xs fs
    inputCommand fs

--inputCommand fs

inputCommand fs = do
     cmd <- getLine
     let args = splitOn " " cmd
     processCommand fs args
main = do
    inputCommand testTree
    --let args = splitOn " " cmd 
