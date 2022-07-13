{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant if" #-}
import Control.Monad (forM, replicateM)
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List
-- import System.Random

data Board = Board{
    cells::[[Int]],
    max::Int
} | Empty deriving (Show, Eq)




replaceMatrix::[[Int]] -> Int -> Int -> Int -> [[Int]]
replaceMatrix m posx posy value = do
    let (x, y:ys) = splitAt posx m
    let (yx, _: yys) = splitAt posy y
    head $ return (x ++ (yx ++ value: yys) : ys)
    

replace::[Int] -> Int -> Int -> [Int]
replace a pos value = do
    let (x,_:ys) = splitAt pos a
    head $ return (x ++ value : ys)


isValidMovement::[[Int]] -> Int -> (Int, Int) -> Bool
isValidMovement board cell movement = do
    let row = fst movement
    let column = snd movement
    if row >= length board || row < 0 || column >= length (board !! 0) || column < 0
        then False
    else if (board !! row !! column) == 0 || (board !! row !! column) == cell+1
        then True
    else False


getAdjacencyList::[[Int]] -> Int -> [(Int, Int)]
getAdjacencyList board cell = do
    let initialPosition = getIndexMatrix board cell 0
    let directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    let movements = map (\x -> (fst initialPosition + fst x, snd initialPosition + snd x)) directions 
    let validMovements = filter (\x -> isValidMovement board cell x) movements
    let succesor = filter (\x -> (board !! fst x !! snd x) > 0) validMovements
    if length succesor > 0
       then head (return succesor)
    else
        head (return validMovements)


getIndexList::[Int] -> Int -> Int -> Int
getIndexList [] _ _ = -1
getIndexList (x: xs) n index | x == n = index | otherwise = getIndexList xs n (index + 1)   


getIndexMatrix::[[Int]] -> Int -> Int -> (Int , Int)
getIndexMatrix [] _ _ = (-1,-1)
getIndexMatrix (x:xs) n index | pos >= 0 = (index , pos)
                                | otherwise =  getIndexMatrix xs n (index + 1)  where pos = getIndexList x n 0

solve :: [[Int]] -> Int -> Int -> [[[Int]]]
solve m max cell = do
    let adj = getAdjacencyList m cell
    if length adj == 0
        then  if cell == max 
            then [m]
            else []
    else
        if m !! (fst(adj !! 0)) !! (snd (adj !! 0)) == cell + 1
            then solve m max (cell + 1)
        else
            if getIndexMatrix m (cell + 1) 0  /= (-1,-1)
                then []
            else
                [
                    sol1 |  x <- adj,
                    let m2 = replaceMatrix m (fst x) (snd x) (cell + 1),
                    sol1 <- solve m2 max (cell + 1)
                ]


while pos len result m max adj cell= do
    if pos == len || result /= []
        then result
    else do
        let x = adj !! pos
        let m2 = replaceMatrix m (fst x) (snd x) (cell + 1)
        while (pos + 1) len (solve_once m2 max (cell + 1)) m max adj cell
    


solve_once :: [[Int]] -> Int -> Int -> [[Int]]
solve_once m max cell = do
    let adj = getAdjacencyList m cell
    if length adj == 0
        then  if cell == max 
            then m
            else []
    else
        if m !! (fst(adj !! 0)) !! (snd (adj !! 0)) == cell + 1
            then solve_once m max (cell + 1)
        else
            if getIndexMatrix m (cell + 1) 0  /= (-1,-1)
                then []
            else do
                let len = length adj
                while 0 len [] m max adj cell

while_cu pos len result m max adj cell= do
    if pos == len || result >= 2
        then result
    else do
        let x = adj !! pos
        let m2 = replaceMatrix m (fst x) (snd x) (cell + 1)
        while_cu (pos + 1) len (result + (check_uniqueness m2 max (cell + 1))) m max adj cell

check_uniqueness:: [[Int]] -> Int -> Int -> Int
check_uniqueness m max cell = do
    let adj = getAdjacencyList m cell
    if length adj == 0
        then  if cell == max 
            then 1
            else 0
    else
        if m !! (fst(adj !! 0)) !! (snd (adj !! 0)) == cell + 1
            then check_uniqueness m max (cell + 1)
        else
            if getIndexMatrix m (cell + 1) 0  /= (-1,-1)
                then 0
            else do
                let len = length adj
                while_cu 0 len 0 m max adj cell

change_seed :: Int -> Int
change_seed x = do
    ((x * 125) + 1) `mod` 4096

randomInRange::Int -> Int -> Int -> Int
randomInRange l r seed = do
    l + (seed `mod` (r - l))

takeRandomFromPositions positions seed =  do
    let pos = randomInRange 0 (length positions) seed
    let s2 = change_seed seed
    ((positions !! pos), ())
    

-- while1 :: (a -> Bool) -> (a -> a) -> a
whileDelete positions m2 max seed amountToDel 
    | amountToDel > 0 && length positions > 0 = do
        let pos = randomInRange 0 (length positions) seed
        let s2 = change_seed seed
        let (x,y) = positions !! pos
        let (xx,_:yy) = splitAt pos positions 
        let positions2 = xx ++ yy
        let val = m2 !! x !! y
        if val == 1 || val == max
            then whileDelete positions2 m2 max s2 amountToDel
        else do
            let m3 = replaceMatrix m2 x y 0
            let uni = check_uniqueness m3 max 1
            if uni >= 2
                then whileDelete positions2 m2 max s2 amountToDel
            else whileDelete positions2 m3 max s2 (amountToDel - 1)
    | otherwise = (m2, seed)
        


gen m amountToDel seed = do
    let r = length m
    let c = length (m !! 0)
    let positions = collectAllBlankPositions m r c
    let max = length positions

    let ((r1,c1), s2) = placeRandom1 m r c seed 1 0 0
    let seed = s2
    let m2 = replaceMatrix m r1 c1 1
    let m = m2

    let m2 = solve_once m max 1
    -- m2
    let (m3, s2) = whileDelete positions m2 max seed amountToDel
    m3

randomCoordinates r c seed = do
    let r1 = randomInRange 0 r seed
    let s2 = change_seed seed
    let c1 = randomInRange 0 c s2
    let seed = change_seed s2
    ((r1,c1), seed)

placeRandom1 m r c seed val r1 c1 | val == 0 = ((r1,c1), seed)
                                  | otherwise = do 
                                    let ((r1,c1),s2) = randomCoordinates r c seed
                                    placeRandom1 m r c s2 (m !! r1 !! c1) r1 c1

collectAllBlankPositions m r c = 
    [
        (r1,c1) | r1 <- [0..(r-1)],
                    c1 <- [0..(c-1)],
                    m !! r1 !! c1  == 0
    ]

-- matrix = [[0, 3, 0],
--           [0, 8, 1],
--           [7, 0, 9]]
-- matrix = [[15, 16, 0, 3],
--           [14, 1, 2, 5],
--           [0, 13, 0, 0],
--           [12, 0, 8, 7]]
-- matrix = [[0, 0, 19, 0, 5],
--           [0, 18, 25, 6, 0],
--           [0, 23, 0, 0, 0],
--           [0, 13, 1, 0, 8],
--           [0, 14, 0, 11, 9]]
matrix = [[0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0]]
-- matrix = [[0, 25, 0, 0, 3, 0, 6, 0],
--           [23, 0, 21, 0, 0, 0, 0, 0],
--           [38, 0, 29, 0, 31, 11, 1, 9],
--           [0, 39, 35, 30, 19, 32, 0, 14],
--           [49, 0, 0, 34, 0, 18, 15, 0],
--           [0, 48, 52, 41, 0, 64, 0, 16],
--           [47, 45, 0, 53, 63, 55, 56, 0],
--           [0, 44, 43, 0, 61, 60, 59, 58]]
printRow matrix pos | pos < length matrix = do 
    print (matrix !! pos)
    printRow matrix (pos + 1)
    | otherwise = print " "
printMatrix matrix = do printRow matrix 0
-- main = juan 5
main = printMatrix(gen matrix 100 53153)
-- main = print(gen matrix 10 414151)
-- main = print(check_uniqueness matrix 64 1)
-- main = print(solve_once matrix 25 1)
-- main = print(replaceMatrix matrix 1 0 7)
-- main = print(replace list 2 7)
-- main = proof

-- main = do
--     print(getAdjacencyList list 1)