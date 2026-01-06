module PlayerMode where
import System.Random (randomRIO)
import Logic        

renderTriplets :: [LetterInfo] -> String
renderTriplets triplets = foldr (\(x,_,z) acc -> acc ++ colorize [x] z) [] sorted
  where sorted = tripletsSorter triplets

gameLoopExpert :: String -> [String] -> [[LetterInfo]] -> [(Char, ColorCode)] -> Int -> Int -> IO ()
gameLoopExpert secretWord _ _ _ 0 _= do
    putStrLn (paintStr "=============================="                 red)
    putStrLn (paintStr "      --- GAME OVER! ---"                       red)
    putStrLn (paintStr "     The word was: " red ++ paintStr secretWord red)
    putStrLn (paintStr "==============================\n"               red)


gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts turnWithLie = do
    putStrLn ( paintStr "\n=============================================================\n" yellow ++ "Attempts left: " ++ show attempts)


    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= 5
        then do
            putStrLn (paintStr "          --- Word must be exactly 5 letters! ---" yellow)
            gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts turnWithLie
        else if guess `notElem` validWords
            then do
                putStrLn (paintStr "         --- This word is not in the word list! ---" yellow)
                gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts turnWithLie 

            else do
                if guess == secretWord
                    then do
                            putStrLn (paintStr "==============================" green)
                            putStrLn (paintStr "       --- YOU WIN! ---" green)
                            putStrLn ("The word was: " ++ paintStr secretWord green)
                            putStrLn (paintStr "==============================\n" green)
                    else do
                        if turnWithLie == 1 then do
                            let maybeLie = generateLie historyOfWords validWords guess secretWord

                            case maybeLie of
                                Nothing -> do
                                    putStrLn (letterPainter guess secretWord)
                                    let newAlphabet = alphabetPainter guess secretWord currentAlphabet
                                    gameLoopExpert secretWord validWords historyOfWords newAlphabet (attempts - 1) (turnWithLie - 1)

                                Just lieTriplets -> do
                                    putStrLn (renderTriplets lieTriplets)
                                    let liePairs = [(char, colour) | (char,_,colour) <- lieTriplets]
                                    let newAlphabet = alphabetPainterHelper liePairs currentAlphabet

                                    gameLoopExpert secretWord validWords (historyOfWords ++ [lieTriplets]) newAlphabet (attempts - 1) (turnWithLie - 1)
                        else do
                            putStrLn (letterPainter guess secretWord)
                            let newAlphabet = alphabetPainter guess secretWord currentAlphabet
                            gameLoopExpert secretWord validWords (historyOfWords ++ [tripleVec guess secretWord]) newAlphabet (attempts - 1) (turnWithLie - 1)


gameLoopMedium :: String -> [String] -> [(Char, ColorCode)] -> Int -> IO ()
gameLoopMedium secretWord _ _ 0 = do
    putStrLn (paintStr "=============================="                 red)
    putStrLn (paintStr "      --- GAME OVER! ---"                       red)
    putStrLn (paintStr "     The word was: " red ++ paintStr secretWord red)
    putStrLn (paintStr "==============================\n"               red)

gameLoopMedium secretWord validWords currentAlphabet attempts = do
    putStrLn ( paintStr "\n=============================================================\n" yellow ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= 5
        then do
            putStrLn (paintStr "          --- Word must be exactly 5 letters! ---" yellow)
            gameLoopMedium secretWord validWords currentAlphabet attempts
        else if guess `notElem` validWords
            then do
                putStrLn (paintStr "         --- This word is not in the word list! ---" yellow)
                gameLoopMedium secretWord validWords currentAlphabet attempts

            else do
                putStrLn (letterPainter guess secretWord)
                let newAlphabet = alphabetPainter guess secretWord currentAlphabet

                if guess == secretWord
                    then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                    else gameLoopMedium secretWord validWords newAlphabet (attempts - 1)

gameLoopEasy :: String -> [String] -> [(Char, ColorCode)] -> Int -> IO ()
gameLoopEasy secretWord _ _ 0 = do
    putStrLn (paintStr "=============================="                 red)
    putStrLn (paintStr "      --- GAME OVER! ---"                       red)
    putStrLn (paintStr "     The word was: " red ++ paintStr secretWord red)
    putStrLn (paintStr "==============================\n"               red)

gameLoopEasy secretWord validWords currentAlphabet attempts = do
    putStrLn ( paintStr "\n=============================================================\n" yellow ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= 5
        then do
            putStrLn (paintStr "          --- Word must be exactly 5 letters! ---" yellow)
            gameLoopEasy secretWord validWords currentAlphabet attempts
        else if guess `notElem` validWords
            then do
                putStrLn (paintStr "         --- This word is not in the word list! ---" yellow)
                gameLoopEasy secretWord validWords currentAlphabet attempts

            else do
                putStrLn (letterPainter guess secretWord)
                let newAlphabet = alphabetPainter guess secretWord currentAlphabet

                if guess == secretWord
                    then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                    else gameLoopEasy secretWord validWords newAlphabet (attempts - 1)

playerMode :: [String] -> Int -> IO ()
playerMode allWords wordCount = do
    randomIndex <- randomRIO (0, wordCount - 1)
    let secretWord = getWordByIndex allWords randomIndex

    let border     = "+======================+"
    let emptySpace = "\n|                      |"

    putStrLn (paintStr border green)
    putStrLn (paintStr "|       Game mode      |" green)
    putStrLn (paintStr (border ++ emptySpace) green)
    putStrLn (paintStr ("|    Pick difficulty   |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (1) Easy         |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (2) Medium       |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (3) Hard         |" ++ emptySpace) green)
    putStrLn (paintStr border green)

    mode <- getLine

    if mode == "1" then do
            putStrLn (paintStr "=============================================================\n" green)
            putStrLn (paintStr "         --- WORDLE GAME STARTED: Easy difficulty ---\n"         green)
            putStrLn (paintStr "=============================================================\n" green)
            gameLoopEasy secretWord allWords alphabetList 6

        else if mode == "2" then do
            putStrLn (paintStr "=============================================================\n" yellow)
            putStrLn (paintStr "         --- WORDLE GAME STARTED: Medium difficulty ---\n"       yellow)
            putStrLn (paintStr "=============================================================\n" yellow)
            gameLoopMedium secretWord allWords alphabetList 6

            else if mode == "3" then do
                putStrLn (paintStr "=============================================================\n" red)
                putStrLn (paintStr "         --- WORDLE GAME STARTED: Hard difficulty ---\n"         red)
                putStrLn (paintStr "=============================================================\n" red)
                rnd_lie <- randomRIO (1, 5)
                gameLoopExpert secretWord allWords [] alphabetList 6 rnd_lie
            else do
                putStrLn (paintStr "\n==============================" red ++ "\n   --- Invalid mode! ---\n" ++ "==============================\n")
                playerMode allWords wordCount
