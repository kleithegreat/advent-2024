parseInput :: String -> IO [String]
parseInput fileName = do
    contents <- readFile fileName
    return $ lines contents

{-
indices:
00 01 02 03 ..
10 11 12 13 ..
20 21 22 23 ..
30 31 32 33 ..
.. .. .. .. ..
-}

-- from left to right order
getRow :: [String] -> Int -> String
getRow [] _ = ""
getRow g i
    | i < 0 || i >= length g = ""
    | otherwise = g !! i

-- from top to bottom order
getColumn :: [String] -> Int -> String
getColumn [] _ = ""
getColumn g i
    | i < 0 || i >= length (getRow g 0) = ""
    | otherwise = head g !! i : getColumn (tail g) i

-- top to bottom, right to left
getDiagonal :: [String] -> Int -> Int -> String
getDiagonal [] _ _ = ""
getDiagonal g i j
    | i < 0 || j < 0 || i >= length g || j >= length (getRow g 0) = ""
    | otherwise = g !! i !! j : getDiagonal g (i+1) (j-1)

-- top to bottom, left to right
getNegDiagonal :: [String] -> Int -> Int -> String
getNegDiagonal [] _ _ = ""
getNegDiagonal g i j
    | i < 0 || j < 0 || i >= length g || j >= length (getRow g 0) = ""
    | otherwise = g !! i !! j : getNegDiagonal g (i+1)(j+1)

countXmas :: String -> Int
countXmas "" = 0
countXmas ('X':'M':'A':'S':rest) = 1 + countXmas rest
countXmas (_:xs) = countXmas xs

solve1 :: [String] -> Int
solve1 g = sum $ map countXmas extracted where
    extracted = rows ++ revRows ++ cols ++ revCols ++ diags ++ revDiags ++ negDiags ++ negRevDiags where
        m = length g - 1
        n = length (head g) - 1
        rows = [getRow g i | i <- [0..m]]
        revRows = map reverse rows
        cols = [getColumn g i | i <- [0..n]]
        revCols = map reverse cols
        diags = [getDiagonal g 0 j | j <- [0..n]] ++ 
                [getDiagonal g i n | i <- [1..m]]
        revDiags = map reverse diags
        negDiags = [getNegDiagonal g 0 j | j <- [0..n]] ++ 
                [getNegDiagonal g i 0 | i <- [1..m]]
        negRevDiags = map reverse negDiags

hasMAS :: String -> Bool
hasMAS "" = False
hasMAS ('M':'A':'S':_) = True
hasMAS ('S':'A':'M':_) = True
hasMAS (x:xs) = hasMAS xs

isXMAS :: [String] -> Int -> Int -> Bool
isXMAS g i j
    | i <= 0 || i >= length g - 1 || j <= 0 || j >= length (head g) - 1 = False
    | g !! i !! j /= 'A' = False
    | otherwise = (hasMAS diag1 || hasMAS (reverse diag1)) && (hasMAS diag2 || hasMAS (reverse diag2)) where
        diag1 = [g !! (i-1) !! (j-1), g !! i !! j, g !! (i+1) !! (j+1)]
        diag2 = [g !! (i-1) !! (j+1), g !! i !! j, g !! (i+1) !! (j-1)]

solve2 :: [String] -> Int
solve2 g = sum [1 | i <- [1..length g - 2], 
                    j <- [1..length (head g) - 2], 
                    isXMAS g i j]

main :: IO()
main = do
    input <- parseInput "input.txt"
    print $ solve1 input
    print $ solve2 input