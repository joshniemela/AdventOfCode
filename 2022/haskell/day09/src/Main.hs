module Main where

import Data.List ( mapAccumL, nub, iterate )

data Move = R Int | L Int | U Int | D Int deriving (Show, Read) 

parse :: String -> [Move]
parse = map read . lines

distance :: (Int, Int) -> (Int, Int) -> (Int, Int)
distance (x, y) (x', y') = (x' - x, y' - y) 

moveHead :: (Int, Int) -> Move -> [(Int, Int)]
moveHead (x, y) (R m) = [(x, y + n) | n <- [1..m]]  
moveHead (x, y) (L m) = [(x, y - n) | n <- [1..m]]
moveHead (x, y) (U m) = [(x + n, y) | n <- [1..m]]
moveHead (x, y) (D m) = [(x - n, y) | n <- [1..m]]




directionToCoords :: [Move] -> [(Int, Int)]
directionToCoords = fst . mapAccumL (\a b -> (a ++ (moveHead (last a) b), a)) [(0, 0)]


move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (x', y') = if 1 < abs a || 1 < abs b then (x + signum a, y + signum b) else (x, y) 
    where (a, b) = distance (x, y) (x', y')

moves :: [(Int, Int)] -> [(Int, Int)]
moves = scanl move (0, 0) 

part1 :: [Move] -> Int
part1 = length . nub . moves . directionToCoords

part2 :: [Move] -> Int
part2 = length . nub . (!! 9) . iterate moves . directionToCoords

main :: IO ()
main = do
    s <- readFile "input"
    contents <- return $ parse s
    print $ part1 contents
    print $ part2 contents


