module Parsing where

import Data.String
import Data.List
import Data.Char

import Values

-- parsing -- 
parseFile :: String -> Either String [Pixel]
parseFile text =
    let cleanedContent = removeEmptyLine $ lines text
    in case getParsed cleanedContent [] of
        Right parsed -> Right parsed
        Left str -> Left str

getParsed :: [String] -> [Pixel] -> Either String [Pixel]
getParsed [] ret = Right ret
getParsed [start] ret = case checkLine start of
    Right line -> case convertLines line of
            Right parsed -> Right $ insertEnd ret parsed
            Left str -> Left str
    Left str -> Left str
getParsed (start:rest) ret = case checkLine start of
    Right line -> case convertLines line of
            Right parsed -> getParsed rest (insertEnd ret parsed)
            Left str -> Left str
    Left str -> Left str

insertEnd :: [Pixel] -> Pixel -> [Pixel]
insertEnd [] toInsert = [toInsert]
insertEnd list toInsert = list ++ [toInsert]

-- check the grammar of the line --
checkLine :: String -> Either String String
checkLine l@('(':rest) =
    let infos = words l
    in if length infos /= 2 || not (checkInfo (head infos))
        || not (checkInfo (infos !! 1))
    then Left "Parsing: One of the lines is invalid"
    else Right l
checkLine _ = Left "Parsing: One of the lines is invalid"

checkInfo :: String -> Bool
checkInfo line
    | head line == '(' && last line == ')' = True
    | otherwise = False

-- remove empty lines 
removeEmptyLine :: [String] -> [String]
removeEmptyLine text
    | isEmptyLine text = removeEmptyLine (delete "" text)
    | otherwise = text

isEmptyLine :: [String] -> Bool
isEmptyLine [] = False
isEmptyLine (start:rest)
    | null start = True
    | otherwise = isEmptyLine rest

-- convert lines --
convertLines :: String -> Either String Pixel
convertLines text = 
    let infos = words text
        positions = head infos \\ "()"
        colors = infos !! 1 \\ "()"
    in case getPositions positions of
       Right pos -> case getColors colors of
           Right col -> Right Pixel{position = pos, color = col}
           Left str -> Left str
       Left str -> Left str

-- convert the positions and colors to [Int] --
getPositions :: [Char] -> Either String [Int]
getPositions pos =
    let sep = words $ map replaceComma pos
    in if length sep == 2 && checkNumbers sep
    then Right [read $ head sep :: Int, read $ sep !! 1 :: Int]
    else Left "Parsing: One of the position is invalid"

getColors :: [Char] -> Either String [Int]
getColors col =
    let sep = words $ map replaceComma col
    in if length sep == 3 && checkNumbers sep
    then case checkShorts [read $ head sep :: Int, read $ sep !! 1 :: Int,
                          read $ sep !! 2 :: Int] of
         Right res -> Right res
         Left str -> Left str
    else Left "Parsing: One of the colors is invalid"

-- check the numbers --
checkNumbers :: [String] -> Bool 
checkNumbers [start] = all isDigit start
checkNumbers (start:rest)
    | all isDigit start = checkNumbers rest
    | otherwise = False

checkShorts :: [Int] -> Either String [Int]
checkShorts l@[a,b,c]
    | a < 0 || b < 0 || c < 0 = 
        Left "Parsing: The color value must be between 0 and 255"
    | a > 255 || b > 255 || c > 255 = 
        Left "Parsing: The color value must be between 0 and 255"
    | otherwise = Right l

replaceComma :: Char -> Char
replaceComma ',' = ' '
replaceComma c = c