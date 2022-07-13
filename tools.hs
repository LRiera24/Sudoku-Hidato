{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant if" #-}
import Control.Monad (forM, replicateM)
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List
import System.Random

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

while_cu pos len 0 m max adj cell= do
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
-- matrix = [[0, 0, 0, 0, 0, 0, 0, 0],
--           [0, 0, 0, 0, 0, 0, 0, 0],
--           [0, 0, 0, 0, 0, 0, 0, 0],
--           [0, 0, 0, 0, 0, 0, 0, 0],
--           [0, 0, 0, 0, 1, 0, 0, 0],
--           [0, 0, 0, 0, 0, 0, 0, 0],
--           [0, 0, 0, 0, 0, 0, 0, 0],
--           [0, 0, 0, 0, 0, 0, 0, 0]]
-- matrix = [[0, 25, 0, 0, 3, 0, 6, 0],
--           [23, 0, 21, 0, 0, 0, 0, 0],
--           [38, 0, 29, 0, 31, 11, 1, 9],
--           [0, 39, 35, 30, 19, 32, 0, 14],
--           [49, 0, 0, 34, 0, 18, 15, 0],
--           [0, 48, 52, 41, 0, -1, 0, 16],
--           [47, 45, 0, 53, 63, 55, 56, 0],
--           [0, 44, 43, 0, 61, 60, 59, 58]]

list = [0, 0, 2, 1]



    

juan x = do
    let y = change_seed x
    let x = y

    print x

main = juan 5
-- main = print(solve_once matrix 64 1)
-- main = print(solve_once matrix 25 1)
-- main = print(replaceMatrix matrix 1 0 7)
-- main = print(replace list 2 7)
-- main = proof

-- main = do
--     print(getAdjacencyList list 1)