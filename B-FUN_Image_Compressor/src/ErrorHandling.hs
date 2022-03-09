module ErrorHandling where

import Data.Char
import Values

-- ERROR HANDLING ARGS --
isFloat :: String -> Bool
isFloat [start]
    | isDigit start = True
    | otherwise = False
isFloat ('.':rest) = isFloat rest
isFloat (start:rest)
    | isDigit start = isFloat rest
    | otherwise = False

checkError :: [String] -> Options -> Options
checkError ("-n":val:rest) opt
    | all isDigit val = checkError rest opt{colors = (read::String -> Int) val}
    | otherwise = checkError rest opt{colors = -1}
checkError ("-l":val:rest) opt
    | isFloat val = checkError rest opt{limit = (read::String -> Float) val}
    | otherwise = checkError rest opt{limit = -1}
checkError ("-f":val:rest) opt = opt{file = val}
checkError (_:val:rest) opt = opt{colors = -1}
checkError _ opt = opt

checkValues :: Options -> Either String Options
checkValues opt
    | colors opt == -1 = Left "The number of colors is not valid"
    | limit opt == -1 = Left "The limit is not valid"
    | file opt == "" = Left "No file was given"
    | otherwise = Right opt