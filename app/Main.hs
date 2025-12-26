module Main where

import System.Random (randomRIO)

import Logic
import PlayerMode
import AiMode

mainMenuLoop :: [String] -> Int -> IO ()
mainMenuLoop allWords wordCount = do
    let border     = "+======================+"
    let emptySpace = "\n|                      |"
    putStrLn (paintStr border green)
    putStrLn (paintStr "|  WELCOME TO WORDLE!  |" green)
    putStrLn (paintStr (border ++ emptySpace) green)
    putStrLn (paintStr ("|  Pick gamemode (1/3) |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (1) Game mode    |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (2) Helper mode  |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (3) Exit         |" ++ emptySpace) green) -- Added Exit option
    putStrLn (paintStr border green)
    
    mode <- getLine

    if mode == "1" then do
        playerMode allWords wordCount
        putStrLn "\nPress Enter to return to menu..."
        _ <- getLine
        mainMenuLoop allWords wordCount
        else if mode == "2" then do
            aiMode allWords
            putStrLn "\nPress Enter to return to menu..."
            _ <- getLine
            mainMenuLoop allWords wordCount
            else if mode == "3" then
                putStrLn "Goodbye!"
            else do
                putStrLn (paintStr "\n==============================" red ++ "\n   --- Invalid mode! ---\n" ++ "==============================\n")
                mainMenuLoop allWords wordCount

main :: IO ()
main = do
    content <- readFile "valid-wordle-words.txt"
    let allWords = lines content
    let wordCount = length allWords

    if wordCount == 0 
        then putStrLn "The file is empty!"
        else do
            -- 2. Start the recursive loop
            mainMenuLoop allWords wordCount
