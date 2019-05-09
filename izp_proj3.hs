import Data.List
import Data.Ord (comparing)
import System.IO

import Data.Text (Text)
import Data.Text.Read
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Obj_t = Obj_t {
                objid :: Int,
                x     :: Float,
                y     :: Float }deriving(Show, Eq, Ord)
data Cluster_t = Cluster_t {
                size     :: Int,
                obj      :: [Obj_t] }deriving(Show, Eq, Ord)

testCarr = [Cluster_t {size=2, obj=[Obj_t {objid=1,x=2.3,y=3.2}, Obj_t {objid=2,x=5.4,y=7.4}]},
            Cluster_t {size=2, obj=[Obj_t {objid=3,x=6.1,y=5.9}, Obj_t {objid=4,x=0.7,y=6.5}]},
            Cluster_t {size=2, obj=[Obj_t {objid=5,x=7.2,y=8.5}, Obj_t {objid=6,x=8.9,y=3.7}]},
            Cluster_t {size=2, obj=[Obj_t {objid=7,x=3.5,y=2.1}, Obj_t {objid=8,x=3.3,y=9.2}]}]

createCluster = Cluster_t {size=0, obj=[]}

clearCluster :: Cluster_t -> Cluster_t
clearCluster c = Cluster_t {size=0, obj=[]}

appendCluster :: Cluster_t -> Obj_t -> Cluster_t
appendCluster c o = Cluster_t {size=((size c)+1), obj=((obj c)++[o])}

sortByObjid :: [Obj_t] -> [Obj_t]
sortByObjid = sortBy (comparing objid)

sortCluster :: Cluster_t -> Cluster_t
sortCluster c = Cluster_t {size = size c, obj = sortByObjid (obj c)}

mergeClusters :: (Cluster_t, Cluster_t) -> Cluster_t
mergeClusters (c1,c2) = Cluster_t {size = (size c1) + (size c2), obj = sortByObjid ((obj c1) ++ (obj c2))}

removeCluster :: [Cluster_t] -> Int -> [Cluster_t]
removeCluster [] _ = []
removeCluster (a:as) i
            | i == 0 = as
            | otherwise = a : removeCluster as (i-1)

objDistance :: Obj_t -> Obj_t -> Float
objDistance o1 o2 = sqrt ((((x o1) - (x o2))**2) + (((y o1) - (y o2))**2))

--clusterDistance :: Cluster_t -> Cluster_t -> Float
--clusterDistance c1 c2 = maximum ([objDistance o1 o2 | o1 <- (obj c1), o2 <- (obj c2)])
clusterDistance :: (Cluster_t, Cluster_t) -> Float
clusterDistance (c1,c2) = maximum ([objDistance o1 o2 | o1 <- (obj c1), o2 <- (obj c2)])

pairs :: [a] -> [(a,a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <-ys]

--removePair x = filter (/= (findNeighbours x)) x
removePair lst = [x | x <- lst, x /= fst (findNeighbours (pairs lst)), x /= snd (findNeighbours (pairs lst))]

minCluster :: (Cluster_t, Cluster_t) -> (Cluster_t, Cluster_t) -> (Cluster_t, Cluster_t)
minCluster a b
        | clusterDistance a > clusterDistance b  = b
        | clusterDistance a < clusterDistance b  = a
        | clusterDistance a == clusterDistance b = a



findNeighbours :: [(Cluster_t, Cluster_t)] -> (Cluster_t, Cluster_t)
findNeighbours [x] = x
findNeighbours (x:xs) = minCluster x (findNeighbours xs)

printCluster c = do
            show c

parse :: Text -> Obj_t
parse text =
    let [txt1, txt2, txt3] = Text.words text
    in Obj_t {objid = read $ Text.unpack txt1, x = read $ Text.unpack txt2, y = read $ Text.unpack txt3} 

loadObj :: [Text] -> [Obj_t]
loadObj lineList = map parse lineList

createClusterList :: [Obj_t] -> [Cluster_t]
createClusterList list = [appendCluster c o | c <- [createCluster], o <- list]


finalMethod :: [Cluster_t] -> Int -> [Cluster_t]
finalMethod _ 0 = error "Too few clusters"
finalMethod [] _ = error "Empty list of clusters"
finalMethod list count = if (length list) == count 
                         then list 
                         else  finalMethod (mergeClusters (findNeighbours (pairs list)) : removePair list) count

main = do
        handle <- openFile "objekty" ReadMode
        contents <- Text.hGetContents handle
        let line = Text.lines contents
        let obj = loadObj line
        hClose handle
        return(finalMethod (createClusterList obj) 2)
