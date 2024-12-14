import qualified Data.Set as Set
import Data.Char (isAlphaNum)

type Position = (Int, Int)

getFreqs :: String -> Set.Set Char
getFreqs = Set.fromList . filter isAlphaNum

getPositions :: [String] -> Char -> [Position]
getPositions grid c = [(x,y) | (row,y) <- zip grid [0..], (ch,x) <- zip row [0..], ch == c]

getAntinodes :: Position -> Position -> (Position, Position)
getAntinodes (x1, y1) (x2, y2) = ((a1, b1), (a2, b2)) where
    dx = x2 - x1
    dy = y2 - y1
    a1 = x1 - dx
    b1 = y1 - dy
    a2 = x2 + dx
    b2 = y2 + dy

isValidPosition :: Position -> Int -> Int -> Bool
isValidPosition (x, y) width height
    | x >= 0 && x < width && y >= 0 && y < height = True
    | otherwise = False

solve1 :: [String] -> Int
solve1 g = let
    frequencies = foldl Set.union Set.empty $ map getFreqs g
    antinodes = concat [[a, b]| c <- Set.toList frequencies, 
                                pos1 <- getPositions g c, 
                                pos2 <- getPositions g c, 
                                pos1 < pos2, 
                                (a, b) <- [getAntinodes pos1 pos2]]
    in Set.size $ Set.fromList $ [p | p <- antinodes, isValidPosition p (length $ head g) (length g)]

main :: IO()
main = do
    input <- readFile "input.txt"
    print $ solve1 $ lines input