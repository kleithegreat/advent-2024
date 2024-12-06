parseInput :: String -> [String]
parseInput = undefined

main :: IO()
main = do
    contents <- readFile "testInput.txt"
    print $ parseInput contents