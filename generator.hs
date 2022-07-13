{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant if" #-}
import Control.Monad (forM, replicateM)
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List

import Tools
import Solver

data Board = Board{
    cells::[[Int]],
    max::Int
} | Empty deriving (Show, Eq)


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

randomCoordinates r c seed = do
    let r1 = randomInRange 0 r seed
    let s2 = change_seed seed
    let c1 = randomInRange 0 c s2
    let seed = change_seed s2
    ((r1,c1), seed)

takeRandomFromPositions positions seed =  do
    let pos = randomInRange 0 (length positions) seed
    let s2 = change_seed seed
    ((positions !! pos), ())
    

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
    let (m3, s2) = whileDelete positions m2 max seed amountToDel
    m3



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