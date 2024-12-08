import Data.List (elem, elemIndex)

type Grid = [String]

parseInput :: String -> Grid
parseInput = lines

data Guard = GuardUp | GuardRight | GuardDown | GuardLeft deriving (Eq, Show)

charToGuard :: Char -> Maybe Guard
charToGuard '^' = Just GuardUp
charToGuard '>' = Just GuardRight
charToGuard 'v' = Just GuardDown
charToGuard '<' = Just GuardLeft
charToGuard _ = Nothing

isGuard :: Char -> Bool
isGuard c = c `elem` "^>v<"

findGuard :: Grid -> (Int, Int, Maybe Guard)
findGuard [] = (0, 0, Nothing)
findGuard (x:xs)
    | any isGuard x = let
        gChar = head $ filter isGuard x
        Just j = elemIndex gChar x
        g = charToGuard gChar
        in (0, j, g)
    | otherwise = let
        (a, b, g) = findGuard xs
        in (1 + a, b, g)

isBlocked :: Grid -> Int -> Int -> Maybe Guard -> Bool
isBlocked g i j (Just GuardUp)
    | i == 0 = False
    | (g !! (i - 1) !! j) == '#' = True
    | otherwise = False
isBlocked g i j (Just GuardRight)
    | j == length (head g) - 1 = False
    | (g !! i !! (j + 1)) == '#' = True
    | otherwise = False
isBlocked g i j (Just GuardDown)
    | i == length g - 1 = False
    | (g !! (i + 1) !! j) == '#' = True
    | otherwise = False
isBlocked g i j (Just GuardLeft)
    | j == 0 = False
    | (g !! i !! (j - 1)) == '#' = True
    | otherwise = False

turnGuard :: Guard -> Guard
turnGuard GuardUp = GuardRight
turnGuard GuardRight = GuardDown
turnGuard GuardDown = GuardLeft
turnGuard GuardLeft = GuardUp

changePos :: Guard -> (Int, Int)
changePos GuardUp = (-1, 0)
changePos GuardRight = (0, 1)
changePos GuardDown = (1, 0)
changePos GuardLeft = (0, -1)

updateMap :: Grid -> Int -> Int -> Guard -> (Grid, Int, Int, Guard)
updateMap g i j gu
    | i < 0 || j < 0 || i >= length g || j >= length (head g) = (g, i, j, gu)
    | otherwise = updateMap g' i' j' gu' where
        (di, dj) = changePos gu
        blocked = isBlocked g i j (Just gu)

        i' = if blocked then i else i + di
        j' = if blocked then j else j + dj
        gu' = if blocked then turnGuard gu else gu
        g' = [[if r == i && c == j then 'X' else cell | (c, cell) <- zip [0..] row] | (r, row) <- zip [0..] g]

countArea :: Grid -> Int
countArea = foldr (\ x -> (+) (sum [1 | c <- x, c == 'X'])) 0

solve1 :: Grid -> Int
solve1 g = let
    (i, j, Just gu) = findGuard g
    (g', _, _, _) = updateMap g i j gu
    in countArea g'

main :: IO()
main = do
    contents <- readFile "input.txt"
    let g = lines contents
    print $ solve1 g