module Main where
import Data.List

-- Check if a string contains n disjunct characters in a row and return the index of the last character
findN :: String -> Int -> Int -> Int
findN (a:b:xs) n i
  | length (nub (take n (a:b:rest))) == n = i+n
  | otherwise = findN (b:rest) n (i+1)

main = do
  s <- readFile "input"
  let part1 = findN s 4 0
  let part2 = findN s 14 0
  
  print $ "Part 1: " ++ show part1
  print $ "Part 2: " ++ show part2
