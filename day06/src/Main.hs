module Main where
import Data.List

-- Check if a string contains n disjunct characters in a row and return the index of the last character
findN :: String -> Int -> Int
findN (a:b:rest) n = if length (nub (take n (a:b:rest))) /= n 
  then findN (b:rest) n + 1 else n

main = do
  s <- readFile "input"
  let part1 = findN s 4
  let part2 = findN s 14
  
  print $ "Part 1: " ++ show part1
  print $ "Part 2: " ++ show part2
