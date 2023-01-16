import System.IO
import Data.List.Split

data Node = Dir String | File String String

data AdjacencyList = ALRecord Int [Int]
data ParrentList = PLRecord Int Int
data IntToNodeMap = INRecord Int Node

fsIntToNodeMap :: [IntToNodeMap]
fsIntToNodeMap = [(INRecord 0 (Dir "/")), (INRecord 1 (Dir "etc")), (INRecord 2 (Dir "mnt")), (INRecord 3 (Dir "home"))]

fsParrentList :: [ParrentList]
fsParrentList = [(PLRecord 0 0), (PLRecord 1 0), (PLRecord 2 0), (PLRecord 3 0)]

fsAdjacencyList :: [AdjacencyList]
fsAdjacencyList = [(ALRecord 0 [1, 2, 3]), (ALRecord 1 []), (ALRecord 2 []), (ALRecord 3 [])]

getNameFromNode :: Node -> String
getNameFromNode (Dir name) = name
getNameFromNode (File name _) = name

decomposeINRecord :: IntToNodeMap -> Node
decomposeINRecord (INRecord id node) = node

decomposePLRecord :: ParrentList -> Int
decomposePLRecord (PLRecord id par) = par

getHeadFromList :: [a] -> a
getHeadFromList (x:xs) = x

getNodeNameFromID :: Int -> String
getNodeNameFromID id = getNameFromNode $ decomposeINRecord (getHeadFromList (filter (\ (INRecord rId n) -> rId == id) fsIntToNodeMap))

decomposeAlRecord :: AdjacencyList -> [Int]
decomposeAlRecord (ALRecord id ls) = ls

getAllNeighbours :: Int -> [Int]
getAllNeighbours id = decomposeAlRecord $ getHeadFromList (filter (\ (ALRecord rId ls) -> rId == id) fsAdjacencyList)

getStringFromListOfIds :: [Int] -> String
getStringFromListOfIds [] = ""
getStringFromListOfIds (x:xs) = (getNodeNameFromID x) ++ " " ++ getStringFromListOfIds xs

lsCmd :: Int -> IO()
lsCmd id = putStrLn (getStringFromListOfIds (getAllNeighbours id))

getParrentOfId :: Int -> Int
getParrentOfId id = decomposePLRecord $ getHeadFromList (filter (\ (PLRecord rId par) -> rId == id) fsParrentList)
