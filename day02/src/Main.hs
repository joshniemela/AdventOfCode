module Main where

data RockPaperScissors = Rock | Paper | Scissors deriving (Show, Eq, Ord)

data Result = Win | Lose | Draw deriving (Show, Eq, Enum)

getWinner :: RockPaperScissors -> RockPaperScissors -> Result
getWinner Rock Scissors = Win
getWinner Paper Rock = Win
getWinner Scissors Paper = Win
getWinner a b
    | a == b = Draw
    | otherwise = Lose