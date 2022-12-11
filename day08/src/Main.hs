module Main where

import Data.List (transpose, reverse, zipWith4)
checkLine :: [Int] -> Int -> [Bool]
checkLine [] _ = []
checkLine (tree:rest) prev
  | tree > prev = True : checkLine rest tree
  | otherwise = False : checkLine rest prev

checkRight :: [[Int]] -> [[Bool]]
checkRight [] = []
checkRight (line:rest) = checkLine line (-1) : checkRight rest

checkLeft :: [[Int]] -> [[Bool]]
checkLeft forest = map reverse $ checkRight $ map reverse forest
checkUp :: [[Int]] -> [[Bool]]
checkUp forest =  transpose $ checkRight $ transpose forest

checkDown :: [[Int]] -> [[Bool]]
checkDown forest = transpose $ checkLeft $ transpose forest


anyOf4:: [Bool] -> [Bool] -> [Bool] -> [Bool] -> [Bool]
anyOf4 a b c d = zipWith4 (\x y z w -> x || y || z || w) a b c d


-- check if any of the 4 directions has a true
checkAll :: [[Int]] -> [[[Bool]]]
checkAll forest = [checkRight forest, checkLeft forest, checkUp forest, checkDown forest]
main :: IO ()
main = do
    s <- readFile "input"
    let forest = map (\x -> read [x] :: Int) <$> lines s
    let result = zipWith4 anyOf4 (checkRight forest) (checkLeft forest) (checkUp forest) (checkDown forest)
    print "Part 1: "
    print $ sum $ map length $ filter (==True) <$> result

