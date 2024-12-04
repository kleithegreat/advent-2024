parseInput :: String -> IO [String]
parseInput fileName = do
    contents <- readFile fileName
    return $ lines contents

getRow :: [String] -> Integer -> String
getRow = undefined

getColumn :: [String] -> Integer -> String
getColumn = undefined

getDiagonal :: [String] -> Integer -> String
getDiagonal = undefined

getNegDiagonal :: [String] -> Integer -> String
getNegDiagonal = undefined

countXmas :: String -> Int
countXmas = undefined

solve1 :: [String] -> Int
solve1 = undefined

main :: IO()
main = do
    input <- parseInput "input.txt"
    print input