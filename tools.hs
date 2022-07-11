{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant if" #-}
import Control.Monad (forM, replicateM)
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List

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
    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
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

solve :: [[Int]] -> Int -> Int -> [[Int]]
solve m max cell = do
    let adj = getAdjacencyList m cell
    if length adj == 0
        then  if cell == max 
            then m
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



proof = do
  let askName i = do
      putStrLn $ "What's your name (" ++ (show i) ++ ")"
      name <- getLine
      return name
  results <- forM [1,2,3] askName
  putStrLn $ "Results = " ++ show results



matrix = [[0, 3, 2],
          [0, 8, 1],
          [7, 0, 9]]
list = [0, 0, 2, 1]


main = print(solve matrix 9 1)
-- main = print(replaceMatrix matrix 1 0 7)
-- main = print(replace list 2 7)
-- main = proof

-- main = do
--     print(getAdjacencyList list 1)