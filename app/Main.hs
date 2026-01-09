module Main where

import Logic ( paintStr, green, red, filterByWordLength )
import PlayerMode ( playerMode )
import AiMode ( aiMode )

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
    putStrLn (paintStr ("|---> (3) Exit         |" ++ emptySpace) green) 
    putStrLn (paintStr border green)
    
    mode <- getLine 

    putStrLn (paintStr border green)
    putStrLn (paintStr "|   Pick word length   |" green)
    putStrLn (paintStr border green)

    inputStr <- getLine
    let wordLength = read inputStr :: Int

    let dictionary = filterByWordLength allWords wordLength

    if mode == "1" then do
        playerMode dictionary wordCount wordLength
        putStrLn "\nPress Enter to return to menu..."
        _ <- getLine
        mainMenuLoop allWords wordCount
        else if mode == "2" then do
            aiMode dictionary wordLength
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
            mainMenuLoop allWords wordCount
