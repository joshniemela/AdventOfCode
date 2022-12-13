module Main where

import Text.Parsec
import Data.List (sort)

data NestedList = Elem Int | List [NestedList] deriving (Show)

parseInput :: Parsec String () [(NestedList, NestedList)]
parseInput = do
    list1 <- parseNestedList
    char '\n'
    list2 <- parseNestedList
    char '\n'
    rest <- parseInput
    return $ (list1, list2) : rest
    <|> do
        char '\n'
        rest <- parseInput
        return $ rest
    <|> do
        return []
    where
        parseNestedList = do
            char '['
            list <- sepBy parseNestedList (char ',')  
            char ']'
            return $ List list
            <|> do
                num <- many1 digit
                return $ Elem (read num)
right :: Either a b -> b
right (Right x) = x

instance Eq NestedList where
  l == r = compare l r == EQ

instance Ord NestedList where
  l `compare` r = case (l, r) of
    (Elem a, Elem b) -> a `compare` b
    (List a, List b) -> a `compare` b
    (Elem {}, List {}) -> List [l] `compare` r
    (List {}, Elem {}) -> l `compare` List [r]


indices :: (a -> Bool) -> [a] -> [Int]
indices p = map fst . filter (p . snd) . zip [1..]


part1 :: [(NestedList, NestedList)] -> Int
part1 = sum . indices (uncurry (<))

part2 :: [(NestedList, NestedList)] -> Int
part2 = product . indices (`elem` dividers) . sort . (dividers <>) . uncurry (<>) . unzip
 where
  dividers =
    [ List [List [Elem 2]]
    , List [List [Elem 6]]
    ]

main = do
    input <- readFile "input"
    let parsed = parse parseInput "" (input) 
    print $ part1 $ right parsed
    print $ part2 $ right parsed

    