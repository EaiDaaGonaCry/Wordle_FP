module PlayerMode where
import System.Random (randomRIO)
import Logic
    ( colorize,
      tripletsSorter,
      paintStr,
      red,
      yellow,
      printAlphabet,
      green,
      generateLie,
      letterPainter,
      alphabetPainter,
      alphabetPainterHelper,
      tripleVec,
      getWordByIndex,
      alphabetList,
      LetterInfo,
      ColorCode, containsGrayLetter, containsYellowLetter, containsGreenLetter, greenLetters, extractGreenPositions, positionSort )        

renderTriplets :: [LetterInfo] -> String
renderTriplets triplets = foldr (\(x,_,z) acc -> acc ++ colorize [x] z) [] sorted
  where sorted = tripletsSorter triplets

gameLoopExpert :: String -> [String] -> [[LetterInfo]] -> [(Char, ColorCode)] -> Int -> Int -> Int -> IO ()
gameLoopExpert secretWord _ _ _ 0 _ _= do
    putStrLn (paintStr "=============================="                 red)
    putStrLn (paintStr "      --- GAME OVER! ---"                       red)
    putStrLn (paintStr "     The word was: " red ++ paintStr secretWord red)
    putStrLn (paintStr "==============================\n"               red)


gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts wordLength turnWithLie = do
    putStrLn ( paintStr "\n=============================================================\n" yellow ++ "Attempts left: " ++ show attempts)


    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= wordLength
        then do
            putStrLn (paintStr ("          --- Word must be exactly "  ++ show wordLength ++ " letters! ---") yellow)
            gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts wordLength turnWithLie
        else if guess `notElem` validWords
            then do
                putStrLn (paintStr "         --- This word is not in the word list! ---" yellow)
                gameLoopExpert secretWord validWords historyOfWords currentAlphabet attempts wordLength turnWithLie 

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
                                    gameLoopExpert secretWord validWords historyOfWords newAlphabet (attempts - 1) wordLength (turnWithLie - 1)

                                Just lieTriplets -> do
                                    putStrLn (renderTriplets lieTriplets)
                                    let liePairs = [(char, colour) | (char,_,colour) <- lieTriplets]
                                    let newAlphabet = alphabetPainterHelper liePairs currentAlphabet

                                    gameLoopExpert secretWord validWords (historyOfWords ++ [lieTriplets]) newAlphabet (attempts - 1) wordLength (turnWithLie - 1)
                        else do
                            putStrLn (letterPainter guess secretWord)
                            let newAlphabet = alphabetPainter guess secretWord currentAlphabet
                            gameLoopExpert secretWord validWords (historyOfWords ++ [tripleVec guess secretWord]) newAlphabet (attempts - 1) wordLength (turnWithLie - 1)




gameLoopMedium secretWord _ _ 0 _ = do
    putStrLn (paintStr "=============================="                 red)
    putStrLn (paintStr "      --- GAME OVER! ---"                       red)
    putStrLn (paintStr "     The word was: " red ++ paintStr secretWord red)
    putStrLn (paintStr "==============================\n"               red)

gameLoopMedium secretWord validWords currentAlphabet attempts wordLength = do
    putStrLn ( paintStr "\n=============================================================\n" yellow ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= wordLength
        then do
            putStrLn (paintStr ("          --- Word must be exactly "  ++ show wordLength ++ " letters! ---") yellow)
            gameLoopMedium secretWord validWords currentAlphabet attempts wordLength
        else if guess `notElem` validWords
            then do
                putStrLn (paintStr "         --- This word is not in the word list! ---" yellow)
                gameLoopMedium secretWord validWords currentAlphabet attempts wordLength

            else do
                putStrLn (letterPainter guess secretWord)
                let newAlphabet = alphabetPainter guess secretWord currentAlphabet

                if guess == secretWord
                    then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                    else gameLoopMedium secretWord validWords newAlphabet (attempts - 1) wordLength


gameLoopEasy secretWord _ _ _ 0 _ = do
    putStrLn (paintStr "=============================="                 red)
    putStrLn (paintStr "      --- GAME OVER! ---"                       red)
    putStrLn (paintStr "     The word was: " red ++ paintStr secretWord red)
    putStrLn (paintStr "==============================\n"               red)

gameLoopEasy secretWord validWords currentAlphabet knownGreens attempts wordLength = do
    putStrLn ( paintStr "\n=============================================================\n" yellow ++ "Attempts left: " ++ show attempts)

    putStr "Keyboard: "
    printAlphabet currentAlphabet

    putStrLn "Enter your guess: "
    guess <- getLine

    if length guess /= wordLength
        then do
            putStrLn (paintStr ("          --- Word must be exactly "  ++ show wordLength ++ " letters! ---") yellow)
            gameLoopEasy secretWord validWords currentAlphabet knownGreens attempts wordLength
        else if guess `notElem` validWords
            then do
                putStrLn (paintStr "         --- This word is not in the word list! ---" yellow)
                gameLoopEasy secretWord validWords currentAlphabet knownGreens attempts wordLength
            else do
                let guessPos = zip guess [0..]
                let grayWarn   = containsGrayLetter guess currentAlphabet
                let yellowWarn = containsYellowLetter guess currentAlphabet
                let greenWarn  = containsGreenLetter guessPos knownGreens

                let warnings = foldr (\x y -> if not (null x) then y ++ "\n" ++ x else y) [] [grayWarn, yellowWarn, greenWarn]

                if not (null warnings)
                    then do
                        putStrLn (paintStr "\n================== Warnings: ==================" red)
                        putStrLn (paintStr warnings red)

                        putStrLn (paintStr "\n======== Do you want to proceed (y/n): ========" red)
                        yOrNo <- getLine
                        if yOrNo == "y"
                            then do
                                putStrLn (letterPainter guess secretWord)
                                let newAlphabet = alphabetPainter guess secretWord currentAlphabet
                                let newGreenLetters = knownGreens ++ extractGreenPositions (tripleVec guess secretWord)
                                if guess == secretWord
                                    then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                                    else gameLoopEasy secretWord validWords newAlphabet newGreenLetters (attempts - 1) wordLength
                        else do gameLoopEasy secretWord validWords currentAlphabet knownGreens attempts wordLength
                    else do
                        putStrLn (letterPainter guess secretWord)
                        let newAlphabet = alphabetPainter guess secretWord currentAlphabet
                        let newGreenLetters = knownGreens ++ extractGreenPositions (tripleVec guess secretWord)
                        if guess == secretWord
                            then putStrLn (paintStr ("\n==============================" ++ "\n      --- YOU WIN! ---\n" ++ "==============================\n") green)
                            else gameLoopEasy secretWord validWords newAlphabet newGreenLetters (attempts - 1) wordLength

playerMode :: [String] -> Int -> Int -> IO ()
playerMode allWords wordCount wordLength = do
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
            gameLoopEasy secretWord allWords alphabetList [] 6 wordLength

        else if mode == "2" then do
            putStrLn (paintStr "=============================================================\n" yellow)
            putStrLn (paintStr "         --- WORDLE GAME STARTED: Medium difficulty ---\n"       yellow)
            putStrLn (paintStr "=============================================================\n" yellow)
            gameLoopMedium secretWord allWords alphabetList 6 wordLength

            else if mode == "3" then do
                putStrLn (paintStr "=============================================================\n" red)
                putStrLn (paintStr "         --- WORDLE GAME STARTED: Hard difficulty ---\n"         red)
                putStrLn (paintStr "=============================================================\n" red)
                rnd_lie <- randomRIO (1, wordLength)
                gameLoopExpert secretWord allWords [] alphabetList 6 wordLength rnd_lie 
            else do
                putStrLn (paintStr "\n==============================" red ++ "\n   --- Invalid mode! ---\n" ++ "==============================\n")
                playerMode allWords wordCount wordLength
