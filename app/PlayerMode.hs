module PlayerMode where
import Logic

gameLoop :: String -> [String] -> [(Char, String)] -> Int -> IO ()
gameLoop secretWord _ _ 0 = do
    putStrLn (paintStr "==============================" red)
    putStrLn (paintStr "       --- GAME OVER! ---" red)
    putStrLn ("The word was: " ++ paintStr secretWord green)
    putStrLn (paintStr "==============================\n" red)

gameLoop secretWord validWords currentAlphabet attempts = do
    putStrLn ("\n==============================\n" ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= 5
        then do
            putStrLn (paintStr "\n --- Word must be exactly 5 letters! ---" yellow)
            gameLoop secretWord validWords currentAlphabet attempts
        else if guess `notElem` validWords
            then do
                putStrLn "Not in word list!"
                gameLoop secretWord validWords currentAlphabet attempts -- Loop with SAME attempts

            else do
                -- 3. Valid Guess: Print Colors and Continue
                putStrLn (letterPainter guess secretWord)
                let newAlphabet = alphabetPainter guess secretWord currentAlphabet

                if guess == secretWord
                    then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                    else gameLoop secretWord validWords newAlphabet (attempts - 1)

