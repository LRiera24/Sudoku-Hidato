module Generator(
placeRandom1
,gen
,collectAllBlankPositions
,randomCoordinates
,takeRandomFromPositions
,whileDelete
,calculateMax
,getLine
)
where


import Control.Monad (forM, replicateM)
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
collectAllNonObstaclesPositions m r c = 
    [
        (r1,c1) | r1 <- [0..(r-1)],
                    c1 <- [0..(c-1)],
                    m !! r1 !! c1  /= -1
    ]

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

calculateMax m = do
    let r = length m
    let c = length (m !! 0)
    let positions = collectAllNonObstaclesPositions m r c
    let max = length positions
    max