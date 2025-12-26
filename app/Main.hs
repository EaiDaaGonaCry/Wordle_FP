module Main where

import System.Random (randomRIO)

import Logic
import PlayerMode
import AiMode


main :: IO ()
main = do

    content <- readFile "valid-wordle-words.txt"
    let allWords = lines content
    let wordCount = length allWords

    if wordCount == 0 
        then putStrLn "The file is empty!"
        else do
        --     randomIndex <- randomRIO (0, wordCount - 1)
        --     let secretWord = getWordByIndex allWords randomIndex
            
        -- --putStrLn ("Picked random index: " ++ show randomIndex)
        -- --putStrLn ("The secret word is: " ++ secretWord)

        --     putStrLn ("\n" ++ "--- WORDLE GAME STARTED ---")
        --     gameLoop secretWord allWords alphabetList 6
            aiLoop allWords

