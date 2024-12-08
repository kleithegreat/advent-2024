import Data.List.Split (splitOn)
import qualified Data.Set as Set

parseInput :: [String] -> [(Integer, [Integer])]
parseInput [] = []
parseInput (x:xs) = let
    split = splitOn ": " x
    target = read $ head split
    nums = map read $ splitOn " " $ split !! 1
    in (target, nums) : parseInput xs

nextVals :: Set.Set Integer -> Integer -> Set.Set Integer
nextVals s n = Set.union (Set.map (+n) s) (Set.map (*n) s)

isSolvable :: Integer -> [Integer] -> Bool
isSolvable _ [] = False
isSolvable t (x:xs) = t `Set.member` foldl nextVals (Set.singleton x) xs

solve1 :: [(Integer, [Integer])] -> Integer
solve1 a = sum [t | (t, vals) <- a, isSolvable t vals]

main :: IO()
main = do
    contents <- readFile "input.txt"
    let input = lines contents
    print $ solve1 $ parseInput input