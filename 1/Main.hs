import Data.List (sort)
import qualified Data.Map as Map

parseInput :: String -> IO ([Int], [Int])
parseInput filename = do
    contents <- readFile filename
    let pairs = map (map read . words) (lines contents)
    return (map head pairs, map last pairs)

solve1 :: [Int] -> [Int] -> Int
solve1 l r = foldl (\a (x, y) -> a + abs (x - y)) 0 (zip (sort l) (sort r))

solve2 :: [Int] -> [Int] -> Int
solve2 l r =
    let counts = foldl (\a x -> Map.insertWith (+) x 1 a) Map.empty r
        score x = x * Map.findWithDefault 0 x counts
    in sum $ map score l

main :: IO ()
main = do
    (leftList, rightList) <- parseInput "input.txt"
    print (solve1 leftList rightList)
    print (solve2 leftList rightList)