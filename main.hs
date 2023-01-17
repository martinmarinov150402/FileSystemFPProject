import System.IO
import Data.List.Split
import Text.Show.Functions

data Node = Dir String | File String String deriving Show

data AdjacencyList = ALRecord Int [Int] 
                    deriving Show
data ParrentList = PLRecord Int Int 
                    deriving Show
data IntToNodeMap = INRecord Int Node 
                    deriving Show

fsIntToNodeMapInitial :: [IntToNodeMap]
fsIntToNodeMapInitial = [(INRecord 0 (Dir "/")), (INRecord 1 (Dir "etc")), (INRecord 2 (Dir "mnt")), (INRecord 3 (Dir "home")), (INRecord 4 (File "bativan.txt" "Bai Ivan")), (INRecord 5 (File "eqk.txt" " e mnogo qk"))]

fsParrentListInitial :: [ParrentList]
fsParrentListInitial = [(PLRecord 0 0), (PLRecord 1 0), (PLRecord 2 0), (PLRecord 3 0), (PLRecord 4 1), (PLRecord 5 1)]

fsAdjacencyListInitial :: [AdjacencyList]
fsAdjacencyListInitial = [(ALRecord 0 [1, 2, 3]), (ALRecord 1 [4, 5]), (ALRecord 2 []), (ALRecord 3 [])]

fst3 :: (a, b, c) -> a
fst3 (f,s,t) = f

snd3 :: (a, b, c) -> b
snd3 (f,s,t) = s

trd3 :: (a, b, c) -> c
trd3 (f,s,t) = t



getNameFromNode :: Node -> String
getNameFromNode (Dir name) = name
getNameFromNode (File name _) = name

decomposeINRecord :: IntToNodeMap -> Node
decomposeINRecord (INRecord id node) = node

getIdFromINRecord :: IntToNodeMap -> Int
getIdFromINRecord (INRecord id node) = id

decomposePLRecord :: ParrentList -> Int
decomposePLRecord (PLRecord id par) = par

getIdFromPLRecord :: ParrentList -> Int
getIdFromPLRecord (PLRecord id par) = id

getHeadFromList :: [a] -> a
getHeadFromList [] = error "Accessed invalid path"
getHeadFromList (x:xs) = x

getNodeNameFromID :: Int -> [IntToNodeMap]-> String
getNodeNameFromID id fsIntToNodeMap = getNameFromNode $ decomposeINRecord (getHeadFromList (filter (\ (INRecord rId n) -> rId == id) fsIntToNodeMap))

getNodeFromID :: Int -> [IntToNodeMap] -> Node
getNodeFromID id fsIntToNodeMap = decomposeINRecord (getHeadFromList (filter (\ (INRecord rId n) -> rId == id) fsIntToNodeMap)) 

decomposeAlRecord :: AdjacencyList -> [Int]
decomposeAlRecord (ALRecord id ls) = ls

getIdFromAlRecord :: AdjacencyList -> Int
getIdFromAlRecord (ALRecord id ls) = id

getAllNeighbours :: Int -> [AdjacencyList] -> [Int]
getAllNeighbours id fsAdjacencyList = decomposeAlRecord $ getHeadFromList (filter (\ (ALRecord rId ls) -> rId == id) fsAdjacencyList)

getStringFromListOfIds :: [Int] -> [IntToNodeMap] -> String
getStringFromListOfIds [] _ = ""
getStringFromListOfIds (x:xs) fsIntToNodeMap = (getNodeNameFromID x fsIntToNodeMap) ++ " " ++ getStringFromListOfIds xs fsIntToNodeMap

lsCmd :: Int -> [AdjacencyList] -> [IntToNodeMap] -> IO()
lsCmd id fsAdjacencyList fsIntToNodeMap = putStrLn (getStringFromListOfIds (getAllNeighbours id fsAdjacencyList) fsIntToNodeMap)

getParrentOfId :: Int -> [ParrentList] -> Int
getParrentOfId id fsParrentList = decomposePLRecord $ getHeadFromList (filter (\ (PLRecord rId par) -> rId == id) fsParrentList)

isAbsolutePath :: String -> Bool
isAbsolutePath (x:xs) = x == '/'

getNodeFromListOfDirs :: [String] -> Int -> [ParrentList] -> [AdjacencyList] -> [IntToNodeMap]  -> Int
getNodeFromListOfDirs [] currentNode _ _ _ = currentNode
getNodeFromListOfDirs (x:xs) currentNode fsParrentList fsAdjacencyList fsIntToNodeMap = case x of ".." -> getNodeFromListOfDirs xs (getParrentOfId currentNode fsParrentList) fsParrentList fsAdjacencyList fsIntToNodeMap
                                                                                                  "." -> getNodeFromListOfDirs xs currentNode fsParrentList fsAdjacencyList fsIntToNodeMap
                                                                                                  otherwise -> getNodeFromListOfDirs xs (getHeadFromList (filter (\ node -> (getNodeNameFromID node fsIntToNodeMap) == x) (getAllNeighbours currentNode fsAdjacencyList))) fsParrentList fsAdjacencyList fsIntToNodeMap

getNodeFromPath :: String -> Int -> [ParrentList] -> [AdjacencyList] -> [IntToNodeMap] -> Int
getNodeFromPath path currentNode fsParrentList fsAdjacencyList fsIntToNodeMap = if isAbsolutePath path then getNodeFromListOfDirs (splitOn "/" path) 0 fsParrentList fsAdjacencyList fsIntToNodeMap else getNodeFromListOfDirs (splitOn "/" path) currentNode fsParrentList fsAdjacencyList fsIntToNodeMap

rmNodeFromPL :: Int -> [ParrentList] -> [ParrentList]
rmNodeFromPL id fsParrentList = filter (\ record -> (getIdFromPLRecord record) /= id) fsParrentList

rmNodeFromIN :: Int -> [IntToNodeMap] -> [IntToNodeMap]
rmNodeFromIN id fsIntToNodeMap = filter (\ record -> (getIdFromINRecord record) /= id) fsIntToNodeMap

rmNodeFromAL :: Int -> [AdjacencyList] -> [AdjacencyList]
rmNodeFromAL id fsAdjacencyList = filter (\ record -> (getIdFromAlRecord record) /= id) fsAdjacencyList

hasInList :: [Int] -> Int -> Bool
hasInList [] _ = False
hasInList (x:xs) i = if i == x then True else hasInList xs i

updateALforRm :: Int -> [AdjacencyList] -> [AdjacencyList]
updateALforRm id fsAdjacencyList = ((ALRecord (getIdFromAlRecord thatNode) (filter (\ cur -> cur /= id) (decomposeAlRecord thatNode))) : otherNodes) where otherNodes = filter (\ record -> (hasInList (decomposeAlRecord record) id) == False) fsAdjacencyList
                                                                                                                                                           thatNode = getHeadFromList (filter (\ record -> (hasInList (decomposeAlRecord record) id) == True) fsAdjacencyList) 

rmCmd :: Int -> String -> [ParrentList] -> [AdjacencyList] -> [IntToNodeMap] -> ([ParrentList], [AdjacencyList], [IntToNodeMap])
rmCmd fs fileName fsParrentList fsAdjacencyList fsIntToNodeMap = ((rmNodeFromPL nodeId fsParrentList), (updateALforRm nodeId (rmNodeFromAL nodeId fsAdjacencyList)) ,(rmNodeFromIN nodeId fsIntToNodeMap)) where nodeId = (getNodeFromPath fileName fs fsParrentList fsAdjacencyList fsIntToNodeMap)

rm :: Int -> [String] -> [ParrentList] -> [AdjacencyList] -> [IntToNodeMap] -> ([ParrentList], [AdjacencyList], [IntToNodeMap])
rm _ [] fsParrentList fsAdjacencyList fsIntToNodeMap = (fsParrentList, fsAdjacencyList, fsIntToNodeMap)
rm fs (x:xs) fsParrentList fsAdjacencyList fsIntToNodeMap = rm fs xs (fst3 res) (snd3 res) (trd3 res) where res = rmCmd fs x fsParrentList fsAdjacencyList fsIntToNodeMap

getFileContent :: Node -> String
getFileContent (File name cont) = cont

catFilesToString :: Int -> [Int] -> [IntToNodeMap] -> String
catFilesToString _ [] _ = ""
catFilesToString fs (x:xs) fsIntToNodeMap = getFileContent (getNodeFromID x fsIntToNodeMap) ++ catFilesToString fs xs fsIntToNodeMap

beforeSign :: [String] -> String -> [String]
beforeSign [] _ = []
beforeSign (x:xs) sign = if x == sign then [] else x : beforeSign xs sign

afterSignHelper :: [String] -> String -> Bool -> [String]
afterSignHelper [] _ _ = []
afterSignHelper (x:xs) sign reached = if x == ">" then afterSignHelper xs sign True else if reached then (x : (afterSignHelper xs sign reached)) else afterSignHelper xs sign reached

getMaxId :: [IntToNodeMap] -> Int
getMaxId [] = 0
getMaxId (x:xs) = max (getIdFromINRecord x) (getMaxId xs) 

addToAl :: Int -> Int -> [AdjacencyList] -> [AdjacencyList]
addToAl id newId fsAdjacencyList = ((ALRecord (getIdFromAlRecord thatNode) (newId : (decomposeAlRecord thatNode))) : otherNodes) where otherNodes = filter (\ record -> (getIdFromAlRecord record /= id)) fsAdjacencyList
                                                                                                                                       thatNode = getHeadFromList (filter (\ record -> getIdFromAlRecord record == id) fsAdjacencyList) 

createNewFile :: Int -> String -> String -> [IntToNodeMap] -> [ParrentList] -> [AdjacencyList] -> IO()
createNewFile fs name cont fsIntToNodeMap fsParrentList fsAdjacencyList = inputCommand fs ((INRecord newId (File name cont)) : fsIntToNodeMap) ((PLRecord newId fs) : fsParrentList) (addToAl fs newId ((ALRecord newId []) : fsAdjacencyList)) where newId = (getMaxId fsIntToNodeMap) + 1

afterSign :: [String] -> String -> [String]
afterSign ls sign = afterSignHelper ls sign False

catCmdProcess :: Int -> [String] -> [IntToNodeMap] -> [ParrentList] -> [AdjacencyList] -> IO()
catCmdProcess fs ls fsIntToNodeMap fsParrentList fsAdjacencyList = if (afterSign ls ">") == [] then putStrLn (catFilesToString fs (map (\ p -> getNodeFromPath p fs fsParrentList fsAdjacencyList fsIntToNodeMap) (beforeSign ls ">") ) fsIntToNodeMap) else createNewFile fs (getHeadFromList (afterSign ls ">")) (catFilesToString fs (map (\ p -> getNodeFromPath p fs fsParrentList fsAdjacencyList fsIntToNodeMap) (beforeSign ls ">") ) fsIntToNodeMap) fsIntToNodeMap fsParrentList fsAdjacencyList

cmdPwd:: Int -> [IntToNodeMap] -> [ParrentList] -> String
cmdPwd 0 fsIntToNodeMap fsParrentList = "/"
cmdPwd fs fsIntToNodeMap fsParrentList = (cmdPwd (getParrentOfId fs fsParrentList) fsIntToNodeMap fsParrentList) ++ (getNodeNameFromID fs fsIntToNodeMap)

concatAllStringsInListWithSpacesBetween :: [String] -> String
concatAllStringsInListWithSpacesBetween [] = ""
concatAllStringsInListWithSpacesBetween (x:xs) = x ++ " " ++ concatAllStringsInListWithSpacesBetween xs

mkfileCmd :: Int -> [String] -> [IntToNodeMap] -> [ParrentList] -> [AdjacencyList] -> IO()
mkfileCmd _ [] _ _ _ = putStrLn("This command requires 2 arguments")
mkfileCmd fs (x:xs) fsIntToNodeMap fsParrentList fsAdjacencyList = if xs == [] then putStrLn("This command requires 2 arguments") else createNewFile fs x (concatAllStringsInListWithSpacesBetween xs) fsIntToNodeMap fsParrentList fsAdjacencyList

processCommand :: Int -> [String] -> [IntToNodeMap] -> [ParrentList] -> [AdjacencyList] -> IO()
processCommand _ [] _ _ _ = error "Empty command"
processCommand fs (x:xs) fsIntToNodeMap fsParrentList fsAdjacencyList = do
    case x of "pwd" -> putStrLn (cmdPwd fs fsIntToNodeMap fsParrentList)
              "cd" -> inputCommand  (getNodeFromPath (head xs) fs fsParrentList fsAdjacencyList fsIntToNodeMap) fsIntToNodeMap fsParrentList fsAdjacencyList
              "ls" -> if xs == [] then lsCmd fs fsAdjacencyList fsIntToNodeMap else lsCmd (getNodeFromPath (getHeadFromList xs) fs fsParrentList fsAdjacencyList fsIntToNodeMap) fsAdjacencyList fsIntToNodeMap
              "rm" -> inputCommand fs (trd3 res) (fst3 res) (snd3 res) where res = (rm fs xs fsParrentList fsAdjacencyList fsIntToNodeMap)
              "cat" -> catCmdProcess fs xs fsIntToNodeMap fsParrentList fsAdjacencyList
              "mkfile" -> mkfileCmd fs xs fsIntToNodeMap fsParrentList fsAdjacencyList
              otherwise -> putStrLn "Unknown command"
    inputCommand fs fsIntToNodeMap fsParrentList fsAdjacencyList



inputCommand :: Int -> [IntToNodeMap] -> [ParrentList] -> [AdjacencyList] -> IO()
inputCommand fs fsIntToNodeMap fsParrentList fsAdjacencyList = do
     cmd <- getLine
     let args = splitOn " " cmd
     processCommand fs args fsIntToNodeMap fsParrentList fsAdjacencyList
main = do
    inputCommand 0 fsIntToNodeMapInitial fsParrentListInitial fsAdjacencyListInitial