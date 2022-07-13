module Tools
(
replaceMatrix
,replace
,getAdjacencyList
,getIndexList
,getIndexMatrix
,change_seed
,randomInRange
,isValidMovement
)
where


replaceMatrix::[[Int]] -> Int -> Int -> Int -> [[Int]]
replaceMatrix m posx posy value = do
    let (x, y:ys) = splitAt posx m
    let (yx, _: yys) = splitAt posy y
    head $ return (x ++ (yx ++ value: yys) : ys)
    

replace::[Int] -> Int -> Int -> [Int]
replace a pos value = do
    let (x,_:ys) = splitAt pos a
    head $ return (x ++ value : ys)

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

isValidMovement::[[Int]] -> Int -> (Int, Int) -> Bool
isValidMovement board cell movement = do
    let row = fst movement
    let column = snd movement
    if row >= length board || row < 0 || column >= length (board !! 0) || column < 0
        then False
    else if (board !! row !! column) == 0 || (board !! row !! column) == cell+1
        then True
    else False


getIndexList::[Int] -> Int -> Int -> Int
getIndexList [] _ _ = -1
getIndexList (x: xs) n index | x == n = index | otherwise = getIndexList xs n (index + 1)   


getIndexMatrix::[[Int]] -> Int -> Int -> (Int , Int)
getIndexMatrix [] _ _ = (-1,-1)
getIndexMatrix (x:xs) n index | pos >= 0 = (index , pos)
                                | otherwise =  getIndexMatrix xs n (index + 1)  where pos = getIndexList x n 0


change_seed :: Int -> Int
change_seed x = do
    ((x * 125) + 1) `mod` 4096

randomInRange::Int -> Int -> Int -> Int
randomInRange l r seed = do
    l + (seed `mod` (r - l))