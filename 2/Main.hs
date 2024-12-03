parseInput :: String -> IO[[Int]]
parseInput file = do
    contents <- readFile file
    return $ map (map read . words) (lines contents)

data Direction = Increasing | Decreasing deriving (Show, Eq)

isSafe :: Direction -> [Int] -> Bool
isSafe _ [] = True
isSafe _ [_] = True
isSafe dir (x:y:xs) = let
    diff = if dir == Increasing then y - x else x - y
    in diff >= 1 && diff <= 3 && isSafe dir (y:xs)

solve1 :: [[Int]] -> Int
solve1 = foldl (\a x -> if isSafe Increasing x || isSafe Decreasing x then a + 1 else a) 0

isTolerable :: Direction -> [Int] -> Bool
isTolerable dir xs = 
    isSafe dir xs ||
    any (\i -> isSafe dir (take i xs ++ drop (i+1) xs)) [0..length xs - 1]

solve2 :: [[Int]] -> Int
solve2 = foldl (\a x -> if isTolerable Increasing x || isTolerable Decreasing x then a + 1 else a) 0

main :: IO()
main = do
    reports <- parseInput "input.txt"
    print $ solve1 reports
    print $ solve2 reports