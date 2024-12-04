import Data.Char

parseInput :: String -> IO String
parseInput fileName = do
    contents <- readFile fileName
    return $ concat $ lines contents

data State = Start | SeenM | SeenMu | SeenMul | SeenOpen | SeenFirstNum | SeenComma | SeenSecondNum | SeenClose

runFSM :: State -> String -> Maybe Integer -> Maybe Integer -> (State, String, Maybe Integer)
runFSM Start ('m':rest) Nothing Nothing = runFSM SeenM rest Nothing Nothing
runFSM Start (_:rest) Nothing Nothing = runFSM Start rest Nothing Nothing
runFSM SeenM ('u':rest) Nothing Nothing = runFSM SeenMu rest Nothing Nothing
runFSM SeenM (_:rest) Nothing Nothing = runFSM Start rest Nothing Nothing
runFSM SeenMu ('l':rest) Nothing Nothing = runFSM SeenMul rest Nothing Nothing
runFSM SeenMu (_:rest) Nothing Nothing = runFSM Start rest Nothing Nothing
runFSM SeenMul ('(':rest) Nothing Nothing = runFSM SeenOpen rest Nothing Nothing
runFSM SeenMul (_:rest) Nothing Nothing = runFSM Start rest Nothing Nothing
runFSM SeenOpen (x:rest) Nothing Nothing
    | isDigit x = runFSM SeenFirstNum rest (Just $ read [x]) Nothing
    | otherwise = runFSM Start rest Nothing Nothing
runFSM SeenFirstNum (x:rest) (Just a) Nothing
    | isDigit x = runFSM SeenFirstNum rest (Just $ read [x] + a * 10) Nothing
    | otherwise = if x == ',' then runFSM SeenComma rest (Just a) Nothing else runFSM Start rest Nothing Nothing
runFSM SeenComma (x:rest) (Just a) Nothing
    | isDigit x = runFSM SeenSecondNum rest (Just a) (Just $ read [x])
    | otherwise = runFSM Start rest Nothing Nothing
runFSM SeenSecondNum (x:rest) (Just a) (Just b)
    | isDigit x = runFSM SeenSecondNum rest (Just a) (Just $ read [x] + b * 10)
    | otherwise = if x == ')' then runFSM SeenClose rest (Just a) (Just b) else runFSM Start rest Nothing Nothing
runFSM SeenClose s (Just a) (Just b) = (Start, s, Just $ a * b)
runFSM _ "" (Just a) (Just b) = (Start, "", Just $ a * b)
runFSM _ "" Nothing Nothing = (Start, "", Just 0)

maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeAdd Nothing Nothing = Nothing
maybeAdd (Just a) Nothing = Just a
maybeAdd Nothing (Just b) = Just b
maybeAdd (Just a) (Just b) = Just $ a + b

solve1 :: String -> Maybe Integer -> Maybe Integer
solve1 "" acc = acc
solve1 s acc = 
    let (newState, rest, res) = runFSM Start s Nothing Nothing
        acc' = maybeAdd acc res
    in solve1 rest acc'

data Condition = Enabled | Disabled deriving (Eq)

filterDisabled :: String -> Condition -> String
filterDisabled "" _ = ""
filterDisabled s state
    | take 4 s == "do()" = filterDisabled (drop 4 s) Enabled
    | take 7 s == "don't()" = filterDisabled (drop 7 s) Disabled
    | state == Enabled = head s : filterDisabled (tail s) Enabled
    | otherwise = filterDisabled (tail s) Disabled

main :: IO()
main = do
    input <- parseInput "input.txt"
    print $ solve1 input Nothing
    print $ solve1 (filterDisabled input Enabled) Nothing