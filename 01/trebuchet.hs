import Data.Char -- For `ord`
import Data.List -- For `isPrefixOf`
import System.IO

-------------------------------------------------------------------------------
-- Core logic for calculating the calibration (part 1)
-------------------------------------------------------------------------------

-- I could use their built-in functions for this, but I thought it would be
-- beneficial to implement it on my own
isCharDigit :: Char -> Bool
isCharDigit x = ('0' <= x) && (x <= '9')

charToInt :: Char -> Int
charToInt x = (ord x) - (ord '0')

getNumsFromLine1 :: String -> [Int]
getNumsFromLine1 line = map charToInt (filter isCharDigit line)

-------------------------------------------------------------------------------
-- Core logic for calculating the calibration (part 2)
-------------------------------------------------------------------------------

-- This didn't work because it is greedy. I.e. "oneight" would be just [1], not [1, 8]
--getNumsFromLine2 :: String -> [Int]
--getNumsFromLine2 ('o':'n':'e'         : restOfLine) = [1] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('t':'w':'o'         : restOfLine) = [2] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('t':'h':'r':'e':'e' : restOfLine) = [3] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('f':'o':'u':'r'     : restOfLine) = [4] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('f':'i':'v':'e'     : restOfLine) = [5] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('s':'i':'x'         : restOfLine) = [6] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('s':'e':'v':'e':'n' : restOfLine) = [7] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('e':'i':'g':'h':'t' : restOfLine) = [8] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 ('n':'i':'n':'e'     : restOfLine) = [9] ++ (getNumsFromLine2 restOfLine)
--getNumsFromLine2 line
--    | length line == 0                   = []
--    | isCharDigit char                   = [charToInt(char)] ++ (getNumsFromLine2 rest)
--    | otherwise                          = [] ++ getNumsFromLine2 rest
--    where char = head line
--          rest = tail line

-- TODO: Would like to clean up number matching part to read from tuple / map
getNumsFromLine2 :: String -> [Int]
getNumsFromLine2 line = getNewDigit line ++ getRemainder restOfLine
    where restOfLine = tail line
          char       = head line
          getNewDigit line
              | length line == 0          = []
              | "one"   `isPrefixOf` line = [1]
              | "two"   `isPrefixOf` line = [2]
              | "three" `isPrefixOf` line = [3]
              | "four"  `isPrefixOf` line = [4]
              | "five"  `isPrefixOf` line = [5]
              | "six"   `isPrefixOf` line = [6]
              | "seven" `isPrefixOf` line = [7]
              | "eight" `isPrefixOf` line = [8]
              | "nine"  `isPrefixOf` line = [9]
              | isCharDigit char          = [charToInt(char)]
              | otherwise                 = []
          getRemainder restOfLine
              | null restOfLine           = []
              | otherwise                 = getNumsFromLine2 restOfLine

-------------------------------------------------------------------------------
-- IO Jazz - reads from stdin, writes to stdout (part 1)
-------------------------------------------------------------------------------

getLineCalibration :: (String -> [Int]) -> String -> Int
getLineCalibration parser line
    | null ints = 0
    | otherwise = firstDigit * 10 + lastDigit
    where ints = parser line
          firstDigit = head ints
          lastDigit  = last ints

getCalibration :: [String] -> Int
--getCalibration lines = sum (map (getLineCalibration getNumsFromLine1) lines)
getCalibration lines = sum (map (getLineCalibration getNumsFromLine2) lines)

main = do
    contents <- getContents
    let lineData = lines contents
    let calibration = getCalibration lineData
    print calibration
