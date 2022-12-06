module Main where
import Data.List
-- import ord
import Data.Char (ord, isUpper)
-- Read line by line the input file
-- and print the result
type Backpack = (String, String)

readlines :: FilePath -> IO [String]
readlines = fmap lines . readFile

getCompartments :: String -> Backpack
getCompartments s = splitAt (length s `div` 2) s

getPriority :: Char -> Int
getPriority c
    | isUpper c = ord c - 38 -- -ord 'A' + 27
    | otherwise = ord c - 96 -- -ord 'a' + 1


-- Get the first char that intersects both strings
getIntersect :: Backpack -> [Char]
getIntersect = uncurry intersect

getNIntersect :: [String] -> Char
getNIntersect = head . foldl1 intersect

groupsOfThree :: [a] -> [[a]]
groupsOfThree [] = []
groupsOfThree (x:y:z:xs) = [x,y,z] : groupsOfThree xs

main :: IO ()
main = do
    input <- readlines "input"

    -- Part 1
    print $ sum $ getPriority . head . getIntersect . getCompartments <$> input

    -- Part 2
    print $ sum $ getPriority . getNIntersect <$> groupsOfThree input
