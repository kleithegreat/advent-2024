import Data.List (elem, elemIndex)

type Grid = [String]

parseInput :: String -> Grid
parseInput = lines

findGuard :: Grid -> (Int, Int)
findGuard [] = (0, 0)
findGuard (x:xs)
    | '^' `elem` x = let
        Just j = elemIndex '^' x
        in (0, j)
    | otherwise = let
        (a, b) = findGuard xs
        in (1 + a, b)

main :: IO()
main = do
    contents <- readFile "testInput.txt"
    let g = lines contents
    print $ findGuard g