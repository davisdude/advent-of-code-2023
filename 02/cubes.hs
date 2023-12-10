import Data.Char -- For `ord`
import System.IO

-------------------------------------------------------------------------------
-- Core logic
-------------------------------------------------------------------------------
data Game = Game { redCubes :: Int
                 , greenCubes :: Int
                 , blueCubes :: Int
                 } deriving Show

-- Would have liked to have overloaded (+), but I couldn't find a clean way
-- that didn't feel clunky :(
addGames :: Game -> Game -> Game
addGames g1 g2 = Game (redCubes g1 + redCubes g2) (greenCubes g1 + greenCubes g2) (blueCubes g1 + blueCubes g2)

isGameValid :: Game -> Game -> Bool
isGameValid limit game = (redCubes game <= redCubes limit)
                      && (greenCubes game <= greenCubes limit)
                      && (blueCubes game <= blueCubes limit)

data GameRecord = GameRecord { games :: [Game]
                             , index :: Int } deriving Show

sumValidGameEntries :: Game -> [GameRecord] -> Int
sumValidGameEntries limit gameRecords = sum $ [
    index currentGame | currentGame <- gameRecords,
    all (==True) $ map (isGameValid limit) (games currentGame)]

getMinPowerSet :: [Game] -> Int
getMinPowerSet games = (maximum reds) * (maximum greens) * (maximum blues)
    where reds = map redCubes games
          greens = map greenCubes games
          blues = map blueCubes games

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

-- Assumes input is a valid number; will produce garbage if given non-numbers
stringToInt :: String -> Int
stringToInt str = sum digits
    where digits = zipWith (*) powers numbers
          powers = map (10^) [0,1..]
          numbers = map (\x -> ord(x) - ord('0')) $ reverse str

splitGameNumberAndGames :: String -> (String, String)
splitGaneNumberAndGames []                      = ("", "")
splitGameNumberAndGames ('G':'a':'m':'e':' ':s) = splitGameNumberAndGames s
splitGameNumberAndGames (':':s)                 = ("", s)
splitGameNumberAndGames (s:ss)                  = ([s] ++ fst f, snd f)
    where f = splitGameNumberAndGames ss

splitAtChar :: Char -> String -> [String]
splitAtChar _ [] = []
splitAtChar c s = [fst f] ++ (splitAtChar c $ snd f)
    where f = splitAtChar' c s
          splitAtChar' :: Char -> String -> (String, String)
          splitAtChar' c s
              | length s == 0 = ("", "")
              | head s == c   = ("", tail s)
              | otherwise     = ([head s] ++ fst f, snd f)
                    where f = splitAtChar' c ss
                          ss = tail s

splitAtSemicolon = splitAtChar ';'
splitAtComma = splitAtChar ','
splitAtSpace = splitAtChar ' '

parseWordPairToGame :: [String] -> Game
parseWordPairToGame ([x, "red"])   = Game (stringToInt x) 0 0
parseWordPairToGame ([x, "green"]) = Game 0 (stringToInt x) 0
parseWordPairToGame ([x, "blue"])  = Game 0 0 (stringToInt x)
parseWordPairToGame ([_, _])       = Game 0 0 0

parseDrawing :: [String] -> Game
parseDrawing drawing = foldl addGames (Game 0 0 0) games
    where games = map parseWordPairToGame pairs
          pairs' = map splitAtSpace drawing
          pairs = map (\y -> filter (\x -> length x /= 0) y) pairs'

parseLine :: String -> GameRecord
parseLine line = GameRecord games gameNumber 
    where (gameNumberStr, rest) = splitGameNumberAndGames line
          gameNumber = stringToInt gameNumberStr
          individualGames = map splitAtComma $ splitAtSemicolon rest
          games = map parseDrawing individualGames

limit = Game 12 13 14

main = do
    contents <- getContents
    let lineData = lines contents
    let records = map parseLine lineData

    -- Part 1
    --let sumOfRecords = sumValidGameEntries limit records
    --print sumOfRecords

    -- Part 2
    let powers = map getMinPowerSet (map games records)
    print $ sum powers
