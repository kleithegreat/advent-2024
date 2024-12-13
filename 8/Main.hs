import qualified Data.Set as Set
import Data.Char (isAlphaNum)

type Position = (Int, Int)

getFreqs :: String -> Set.Set Char
getFreqs = Set.fromList . filter isAlphaNum

solve1 :: [String] -> Int
solve1 = undefined

main :: IO()
main = do
    input <- readFile "testInput.txt"
    print $ lines input