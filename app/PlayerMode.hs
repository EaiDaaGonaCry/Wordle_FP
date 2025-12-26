module PlayerMode where
import System.Random (randomRIO)
import Logic

renderTriplets :: [(Char, Int, String)] -> String
renderTriplets triplets = foldr (\(x,_,z) acc -> acc ++ colorize [x] z) [] sorted
  where sorted = tripletsSorter triplets

gameLoopExpert :: String -> [String] -> [[(Char, Int, String)]] -> [(Char, String)] -> Int -> Int -> IO ()
gameLoopExpert secretWord _ _ _ 0 _= do
    putStrLn (paintStr "==============================" red)
    putStrLn (paintStr "       --- GAME OVER! ---" red)
    putStrLn ("The word was: " ++ paintStr secretWord green)
    putStrLn (paintStr "==============================\n" red)

gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts turnWithLie = do
    putStrLn ("\n==============================\n" ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= 5
        then do
            putStrLn (paintStr "\n --- Word must be exactly 5 letters! ---" yellow)
            gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts turnWithLie
        else if guess `notElem` validWords
            then do
                putStrLn "Not in word list!"
                gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts turnWithLie 

            else do
                if guess == secretWord
                    then do
                            putStrLn (paintStr "==============================" green)
                            putStrLn (paintStr "       --- YOU WIN! ---" green)
                            putStrLn ("The word was: " ++ paintStr secretWord green)
                            putStrLn (paintStr "==============================\n" green)
                    else do
                        -- Lie blocking logic
                        if turnWithLie == 1 then do
                            -- let test1 = generateLie historyOfWords validWords guess secretWord
                            -- let test2 = generateLieDABNGFADBUAD historyOfWords validWords guess secretWord
                            -- let ltest1 = length test1
                            -- let ltest2 = length test2
                            -- putStrLn ("Length of maybeLie: " ++ show ltest1 ++ renderTriplets (head test1)++ " " ++ renderTriplets (head (tail  test1)))
                            -- putStrLn ("Length of maybeLie: " ++ show ltest2 ++ renderTriplets (head test2)++ " " ++ renderTriplets (head (tail  test2)))
                            let maybeLie = generateLie historyOfWords validWords guess secretWord

                            case maybeLie of
                                Nothing -> do
                                    --putStrLn ("No word")
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


gameLoopMedium :: String -> [String] -> [(Char, String)] -> Int -> IO ()
gameLoopMedium secretWord _ _ 0 = do
    putStrLn (paintStr "==============================" red)
    putStrLn (paintStr "       --- GAME OVER! ---" red)
    putStrLn ("The word was: " ++ paintStr secretWord green)
    putStrLn (paintStr "==============================\n" red)

gameLoopMedium secretWord validWords currentAlphabet attempts = do
    putStrLn ("\n==============================\n" ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= 5
        then do
            putStrLn (paintStr "\n --- Word must be exactly 5 letters! ---" yellow)
            gameLoopMedium secretWord validWords currentAlphabet attempts
        else if guess `notElem` validWords
            then do
                putStrLn "Not in word list!"
                gameLoopMedium secretWord validWords currentAlphabet attempts

            else do
                putStrLn (letterPainter guess secretWord)
                let newAlphabet = alphabetPainter guess secretWord currentAlphabet

                if guess == secretWord
                    then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                    else gameLoopMedium secretWord validWords newAlphabet (attempts - 1)

playerMode :: [String] -> Int -> IO ()
playerMode allWords wordCount = do
    randomIndex <- randomRIO (0, wordCount - 1)
    let secretWord = getWordByIndex allWords randomIndex
            
    putStrLn ("Picked random index: " ++ show randomIndex)
    putStrLn ("The secret word is: " ++ secretWord)

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
            putStrLn ("\n" ++ "--- WORDLE GAME STARTED: Easy difficulty ---")
            gameLoopMedium secretWord allWords alphabetList 6

        else if mode == "2" then do
            putStrLn ("\n" ++ "--- WORDLE GAME STARTED: Medium difficulty---")
            gameLoopMedium secretWord allWords alphabetList 6
            else if mode == "3" then do
                putStrLn ("\n" ++ "--- WORDLE GAME STARTED: Hard difficulty---")
                rnd_lie <- randomRIO (1, 5)
                gameLoopExpert secretWord allWords [] alphabetList 6 rnd_lie
            else do
                putStrLn (paintStr "\n==============================" red ++ "\n   --- Invalid mode! ---\n" ++ "==============================\n")
