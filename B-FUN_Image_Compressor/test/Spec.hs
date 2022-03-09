import Test.HUnit

import ErrorHandling
import Clusters
import Parsing
import Values

testError :: Test
testError = TestList [
    -- isFloat --
    "invalid float" ~: False ~=? isFloat "-6.9",
    "valid float" ~: True ~=? isFloat "6.9",
    -- checkError --
    "invalid number of clusters" ~: -1 ~=? colors (checkError ["-6", "0.8", "file.txt"] (Options (-1) (-1) "")),
    "invalid limit" ~: -1 ~=? limit (checkError ["6", "-0.8", "file.txt"] (Options (-1) (-1) "")),
    "invalid file" ~: -1 ~=? limit (checkError ["6", "0.8", ""] (Options (-1) (-1) ""))
    ]

testCompute :: Test
testCompute = TestList [
    -- computeDist --
    "Distance (3,4,5) (3,8,2)" ~: 5.0 ~=? computeDist [3,4,5] [3,8,2],
    "Distance (1,1,1) (1,1,1)" ~: 0.0 ~=? computeDist [1,1,1] [1,1,1]
    ]

testParsing :: Test
testParsing = TestList [
    -- checkInfo --
    "invalid check" ~: False ~=? checkInfo "5,6",
    "valid check" ~: True ~=? checkInfo "(5,6)",
    -- isEmptyLine --
    "not empty line" ~: False ~=? isEmptyLine ["5,6", "8,9"],
    "not empty line" ~: True ~=? isEmptyLine ["5,6", ""],
    -- removeEmptyLine --
    "remove all empty lines" ~: ["6", "8", "10"] ~=? removeEmptyLine ["6", "", "8", "", "10"],
    -- replaceComma --
    "replace commas" ~: ' ' ~=? replaceComma ',',
    -- checkNumbers --
    "check numbers parsing OK" ~: True ~=? checkNumbers ["23", "78", "8"],
    "check numbers parsing KO" ~: False ~=? checkNumbers ["23", "78", "-8"]
    ]

main :: IO Counts 
main =
    runTestTT testError >>
    runTestTT testCompute >>
    runTestTT testParsing