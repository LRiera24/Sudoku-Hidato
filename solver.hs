module Solver(
solve
,while_so
,while_cu
,solve_once
,check_uniqueness
) 
where

import Tools



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

while_so pos len result m max adj cell= do
    if pos == len || result /= []
        then result
    else do
        let x = adj !! pos
        let m2 = replaceMatrix m (fst x) (snd x) (cell + 1)
        while_so (pos + 1) len (solve_once m2 max (cell + 1)) m max adj cell
    
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
                while_so 0 len [] m max adj cell

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


