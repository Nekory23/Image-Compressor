--module Main where

import System.Environment ( getArgs )
import System.IO
import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith, exitFailure )
import Data.Char ( isDigit )

import ErrorHandling
import Values
import Parsing
import Clusters

displayError :: String -> IO a
displayError error = 
    hPutStrLn stderr usage >> hPutStrLn stderr ("Error: " ++ error)
    >> exitWith (ExitFailure 84)

printClusterColor :: [Int] -> IO ()
printClusterColor [r,g,b] =
    putStrLn ("(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")
    >> return ()

getPositionLine :: Pixel -> String
getPositionLine c =
    "(" ++ show (head (position c)) ++ "," ++ show (position c !! 1) ++ ") "

getColorLine :: Pixel -> String
getColorLine c =
    "(" ++ show (head (color c)) ++ "," ++ show (color c !! 1) ++ ","
    ++ show (color c !! 2) ++ ")"

printPoints :: [Pixel] -> IO ()
printPoints [] = return ()
printPoints [start] = 
    let position = getPositionLine start
        colors = getColorLine start
    in putStr position >> putStrLn colors >> return ()
printPoints (start:rest) = 
    let position = getPositionLine start
        colors = getColorLine start
    in putStr position >> putStrLn colors >> printPoints rest

printClusters :: [Cluster] -> IO ()
printClusters [] = return ()
printClusters [start] = 
    putStrLn "--" >> printClusterColor (clusterColor start) >>
    putStrLn "-" >> printPoints (points start) >> return ()
printClusters (start:rest) =
    putStrLn "--" >> printClusterColor (clusterColor start) >>
    putStrLn "-" >> printPoints (points start) >> printClusters rest

startAlgo :: [Pixel] -> Options -> IO ()
startAlgo contents opt = do
    clusters <- initClusters [] contents (colors opt)
    let setClusters = setPoints clusters contents
        in printClusters $ kMeans setClusters opt contents

main :: IO ()
main = do
    args <- getArgs
    let arg = checkError args (Options (-1) (-1) "")
    case checkValues arg of
        Right opt -> do 
            c <- readFile (file opt) 
            case parseFile c of
                Right contents ->
                    startAlgo contents opt
                Left str -> displayError str
        Left str -> displayError str