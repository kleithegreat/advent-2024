import Data.List.Split (splitOn)
import Data.List (elemIndex)

type Rule = (Integer, Integer)
type Update = [Integer]

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

isValid :: Rule -> Update -> Bool
isValid _ [] = True
isValid (f, l) u = case (fi, li) of
  (_, Nothing) -> True
  (Nothing, _) -> True
  (Just a, Just b) -> a < b
  where
    fi = elemIndex f u
    li = elemIndex l u

getMiddle :: Update -> Integer
getMiddle u = u !! (length u `div` 2)

solve1 :: [Rule] -> [Update] -> Integer
solve1 rs us = sum [getMiddle u | u <- us, all (`isValid` u) rs]

main :: IO()
main = do
    content <- readFile "input.txt"
    let (rules, updates) = parseInput content
    print $ solve1 rules updates