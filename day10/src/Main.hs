module Main where
import Text.Parsec
data CPU = CPU {register :: Int, programQueue :: [Int]} deriving (Show)
data Instruction = Noop | AddX Int deriving (Show)


-- parse addx -14 to AddX -14 and noop to Noop
parseInstruction :: String -> Instruction
parseInstruction s = do
  let instruction = take 4 s
  case instruction of
    "addx" -> AddX (read (drop 5 s) :: Int)
    "noop" -> Noop

cycleCPU :: CPU -> CPU
cycleCPU cpu = CPU (register cpu + head (programQueue cpu)) (tail (programQueue cpu) ++ [0])
execute :: Instruction -> CPU -> CPU
execute Noop cpu = cpu
execute (AddX x) cpu = CPU (register cpu) (replaceAt 1 x (programQueue cpu))

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = take n xs ++ [x] ++ drop (n+1) xs

main :: IO ()

-- filter cycles, get the 20th
every40th :: [a] -> [a]
every40th [] = []
every40th (x:xs) = x : every40th (drop 39 xs)

readlines :: FilePath -> IO [String]
readlines = fmap lines . readFile
main = do
  s <- readlines "inputtest"
  let instructions = map parseInstruction s
  --print instructions
  print instructions
  let cpu = CPU {register = 1, programQueue = [0,0,0]}
  -- execute CPU instruction and then cycle
  let cpu' = scanl (flip execute . cycleCPU) cpu instructions 
  let cpu'' = drop 1 cpu' ++ [cycleCPU (last cpu')]

  -- get every 40th cycle and the 20th cycle
  let cpucycles = [cpu'' !! 19] ++ every40th (drop 40 cpu'')
  print $ map register cpu''
