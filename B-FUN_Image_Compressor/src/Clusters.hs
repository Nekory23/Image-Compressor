module Clusters where

import System.Random
import Values

-- init the clusters --
getCluster :: [Pixel] -> IO [Int]
getCluster content = do
    nbr <- randomRIO (0, length content - 1)
    return (color (content !! nbr))

checkCentroids :: [Cluster] -> [Int] -> Bool
checkCentroids [] _ = True
checkCentroids [start] [r,g,b] =
    let col = clusterColor start
    in not (head col == r && col !! 1 == g && col !! 2 == b)
checkCentroids (start:rest) check@[r,g,b] =
    let col = clusterColor start
    in not (head col == r && col !! 1 == g && col !! 2 == b) 
        && checkCentroids rest check

initClusters :: [Cluster] -> [Pixel] -> Int -> IO [Cluster]
initClusters c cont nbr
    | length c == nbr = return c
    | otherwise = do
        new <- getCluster cont
        let newCluster = Cluster{clusterColor = new, points = []}
            in if checkCentroids c new
               then initClusters (c ++ [newCluster]) cont nbr
               else initClusters c cont nbr

-- get the distance between the clusters and a point --
computeDist :: [Int] -> [Int] -> Float
computeDist [cr,cg,cb] [pr,pg,pb] =
    let x2 = (pr - cr) ^ 2
        y2 = (pg - cg) ^ 2
        z2 = (pb - cb) ^ 2
        res = sqrt $ fromIntegral (x2 + y2 + z2)
    in res

getDist :: [Cluster] -> Pixel -> [Float] -> [Float]
getDist [start] content ret =
    let dist = computeDist (clusterColor start) (color content)
    in (ret ++ [dist])
getDist (start:rest) content ret =
    let dist = computeDist (clusterColor start) (color content)
    in getDist rest content (ret ++ [dist])

-- assign the value to the closest cluster --
addPoint :: Cluster -> Pixel -> Cluster
addPoint c point = c{points = points c ++ [point]}

findSmallestDist :: [Float] -> Float -> Int -> Int
findSmallestDist list@(start:rest) min ret
    | start == min = ret
    | otherwise = findSmallestDist rest min (ret + 1)

replaceValue :: [Cluster] -> Cluster -> Int -> [Cluster]
replaceValue [] _ _ = []
replaceValue (start:rest) new 0 = new:rest
replaceValue (start:rest) new nbr = start:replaceValue rest new (nbr - 1)

assignToCluster :: [Cluster] -> Pixel -> [Float] -> [Cluster]
assignToCluster c pixel dist =
    let nbr = findSmallestDist dist (minimum dist) 0
        nCluster = addPoint (c !! nbr) pixel
    in replaceValue c nCluster nbr

setPoints :: [Cluster] -> [Pixel] -> [Cluster]
setPoints c [start] =
        let dist = getDist c start []
        in assignToCluster c start dist
setPoints c (start:rest) =
    let dist = getDist c start []
    in setPoints (assignToCluster c start dist) rest

-- loop --
-- convert the floats to int --
--convertToInt :: [Float] -> [Int]
--convertToInt [r,g,b] = [round r, round g, round b]

computeMean :: [Pixel] -> [Int] -> Int -> [Int]
computeMean [start] [r,g,b] nb =
    let mean = [(r + head (color start)) `div` nb,
                (g + color start !! 1) `div` nb,
                (b + color start !! 2) `div` nb]
    in mean
computeMean (start:rest) [r,g,b] nb =
    computeMean rest [r + head (color start), g + color start !! 1,
                      b + color start !! 2] nb

computeCluster :: Cluster -> Cluster
computeCluster old =
    let newColor = computeMean (points old) [0,0,0] (length (points old))
    in Cluster{clusterColor = newColor, points = []}

getNewClusters :: [Cluster] -> [Cluster] -> [Cluster]
getNewClusters [start] nCluster =
    let new = computeCluster start
    in (nCluster ++ [new])
getNewClusters c@(start:rest) nCluster =
    let new = computeCluster start
    in getNewClusters rest (nCluster ++ [new])

getDistCentroids :: [Cluster] -> [Cluster] -> [Float] -> [Float]
getDistCentroids [a] [b] dist =
    let new = computeDist (clusterColor a) (clusterColor b)
    in (dist ++ [new])
getDistCentroids (a:as) (b:bs) dist =
    let new = computeDist (clusterColor a) (clusterColor b)
    in getDistCentroids as bs (dist ++ [new])

checkDistCentroids :: [Float] -> Float -> Bool
checkDistCentroids [start] comp
    | start <= comp = True
    | otherwise = False
checkDistCentroids (start:rest) comp
    | start <= comp = checkDistCentroids rest comp
    | otherwise = False
    

kMeans :: [Cluster] -> Options -> [Pixel] -> [Cluster]
kMeans cluster opt contents =
    let newClusters = getNewClusters cluster []
        dist = getDistCentroids cluster newClusters []
    in if checkDistCentroids dist (limit opt)
       then cluster
       else setPoints newClusters contents