import System.IO
import Control.Monad (forM, replicateM)
import Solver
import Generator
import Tools

main = do
    putStrLn "Hello. Ready to play Hidato?"
    putStrLn "Select on of the options below by typing the corresponding number and pressing enter."
    putStrLn "(1) I want to create a new Hidato."
    putStrLn "(2) I want to introduce an existing one in order to be solved."
    line <- getLine
    let n = read line :: Int
    if n == 1
        then generateAndSolve
    else if n == 2
        then solver
    else do
        putStrLn "You must select on of the previous cases. Let start over."
        putStrLn ""
        main

    
intmap :: [String] -> [[Int]]
intmap x = map (\l -> map read (words l)) x

getStringMatrix::Int -> Int
getStringMatrix x = x


solver = do
    putStrLn "Please give me two numbers representing the dimensions of the hidato."
    line <- getLine
    let (x:y:_) = map (\s -> read s :: Int) (words line)

    putStrLn "Now please enter the hidato as a space separated matrix."
    putStrLn "Note: The form must be a rectangle possibly containing -1 squares, meaning spaces out of the hidato."
    putStrLn "Note: Use 0 for the blank spaces to solve."
    mStr <- forM [1..(x)] (\a -> do line <- getLine
                                    return line)
    let m = intmap mStr
    let max = calculateMax m 
    putStrLn ""
    putStrLn "Here is the solution:"
    putStrLn ""
    printMatrix (solve_once m max 1 )

    putStrLn ""
    putStrLn "We hope you are happy with the result !!!"


generateAndSolve = do
    putStrLn "Please give me two numbers representing the dimensions of the smaller rectangle containing the desire shape of the hidato."
    line <- getLine
    let (x:y:_) = map (\s -> read s :: Int) (words line) 

    putStrLn "Now please enter the shape of the hidato as a space separated matrix."
    putStrLn "Note: The form must be a rectangle possibly containing -1 squares, meaning spaces out of the hidato."
    putStrLn "Note: Use 0 everywhere else."

    mStr <- forM [1..(x)] (\a -> do line <- getLine
                                    return line)
    let m = intmap mStr

    putStrLn "Please introduce the amount of blank spaces you wish to have in the hidato (i.e. the dificulty)"
    line <- getLine
    let toDelete = read line :: Int   

    putStrLn "Now introduce a seed for the random generator. (Any number of at least 4 digits)"
    line2 <- getLine
    let seed = read line2 :: Int  

    let m2 = gen m toDelete seed
    printMatrix m2

    putStrLn "Would you wish to solve this Hidato? ('Y' or 'N')"
    putStrLn ""
    
    c <- getChar

    if c == 'Y' || c == 'y'
        then do
            putStrLn ""
            putStrLn "Here is the solution:"
            putStrLn ""
            let max = calculateMax m2 
            printMatrix (solve_once m2 max 1)
        else 
            putStrLn ""
    
    putStrLn "We hope you are happy with the result !!!"

    
    
