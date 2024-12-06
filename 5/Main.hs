import Data.List.Split (splitOn)

type Rule = (Int, Int)
type Update = [Int]

parseInput :: String -> ([Rule], [Update])
parseInput input = (rules, updates)
  where
    [rulesStr, updatesStr] = splitOn "\n\n" $ filter (/= '\r') input
    rules = map parseRule $ lines rulesStr
    updates = map parseUpdate $ lines updatesStr

parseRule :: String -> Rule
parseRule s = (read x, read y)
  where
    [x,y] = splitOn "|" s

parseUpdate :: String -> Update
parseUpdate = map read . splitOn ","

main :: IO()
main = do
    content <- readFile "testInput.txt"
    let (rules, updates) = parseInput content
    print rules