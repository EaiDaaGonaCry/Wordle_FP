module AiMode where
import Logic
import System.Random (randomRIO)


aiLoopExpert :: [ScoredWord] -> Int -> IO ()
aiLoopExpert dictionary attempt = do
    if null dictionary 
        then putStrLn (paintStr ("\n===============================" ++ "\n--- No possible words left! ---\n" ++ "===============================\n") red)
        else if length dictionary == 1 
            then do 
                putStrLn (paintStr "==================================" green)
                putStrLn (paintStr ("---Your word is: " ++ fst (head dictionary) ++ " ---") green)
                putStrLn (paintStr "==================================" green)
            else do
                if length dictionary > 3000 
                    then do 
                        let currWords = map fst dictionary
                        let wordCount = length currWords
                        rnd <- randomRIO (0, wordCount - 1)
                        let guessWord = getWordByIndex currWords rnd
                        
                        processGuessExpert dictionary guessWord attempt
                    else do 
                        let currWords = map fst dictionary
                        let scores = [ (word, scoreCount word currWords) | word <- currWords ]
                        let maxScore = maximum [ score | (_, score) <- scores ]
                        let bestGuesses = [ word | (word, score) <- scores , score == maxScore ]
                        let chosenWord = head bestGuesses

                        processGuessExpert dictionary chosenWord attempt

processGuessExpert :: [ScoredWord] -> String -> Int -> IO ()
processGuessExpert dictionary guessWord attempt = do
    putStrLn (paintStr "=================================="                         yellow)
    putStrLn (paintStr ("---      Attempt number " ++ show attempt ++ ":     ---")   yellow)
    putStrLn (paintStr ("--- Is your word: "       ++ guessWord   ++ " ? (y/n)---") yellow)
    putStrLn (paintStr "=================================="                         yellow)
    
    response <- getLine
    if response == "y"
        then putStrLn (paintStr ("\n==============================" ++ "\n       --- AI WIN! ---\n" ++ "==============================\n") green)
        else do
            putStrLn ("Enter the pattern (g -" ++ paintStr "green" green ++ ", y - " ++ paintStr "yellow" yellow ++ ", x- " ++ paintStr "gray" gray ++ ")")
            colourLine <- getLine
    
            if length colourLine /= 5
                then do
                    putStrLn (paintStr "          --- Word must be exactly 5 letters! ---" yellow)
                    processGuessExpert dictionary guessWord attempt
                else do
                    let parsedColors = charsToColours colourLine
                    let newDictionary = filter (\(w, _) -> w /= guessWord) (updateCandidateWords dictionary guessWord parsedColors)
                    aiLoopExpert newDictionary (attempt + 1)


aiLoopNormal :: [String] -> String -> Int -> IO ()
aiLoopNormal [] _ _ =  putStrLn (paintStr ("\n===============================" ++ "\n--- No possible words left! ---\n" ++ "===============================\n") red)
aiLoopNormal dictionary lastPattern attempt = do
    if lastPattern == "xxxxx" 
        then do
            let wordCount = length dictionary
            rnd <- randomRIO (0, wordCount - 1)
            let guessWord = getWordByIndex dictionary rnd
            processGuessNormal dictionary guessWord attempt
        else do
            let scores = [ (word, scoreCount word dictionary) | word <- dictionary ]
            let maxScore = maximum [ score | (_, score) <- scores ]
            let candidates = [ word | (word, score) <- scores , score == maxScore ]
            let chosenWord = head candidates
            processGuessNormal dictionary chosenWord attempt

processGuessNormal :: [String] -> String -> Int -> IO ()
processGuessNormal dictionary guessWord attempt = do
    putStrLn (paintStr "=================================="                         yellow)
    putStrLn (paintStr ("---      Attempt number " ++ show attempt ++ ":     ---")   yellow)
    putStrLn (paintStr ("--- Is your word: "       ++ guessWord   ++ " ? (y/n)---")  yellow)
    putStrLn (paintStr "=================================="                         yellow)
    
    response <- getLine
    if response == "y"
        then  putStrLn (paintStr ("\n==============================" ++ "\n      --- AI WIN! ---\n" ++ "==============================\n") green)
        else do
            putStrLn ("Enter the pattern (g -" ++ paintStr "green" green ++ ", y - " ++ paintStr "yellow" yellow ++ ", x- " ++ paintStr "gray" gray ++ ")")
            colourLine <- getLine
            
            if length colourLine /= 5
                then do
                     putStrLn (paintStr "          --- Word must be exactly 5 letters! ---" yellow)
                     processGuessNormal dictionary guessWord attempt
                else do
                    let triples = parserTriplets guessWord colourLine
                    let filteredDict = filterColours dictionary triples
                    let newDictionary = filter (/= guessWord) filteredDict
                    aiLoopNormal newDictionary colourLine (attempt + 1)


aiMode :: [String] -> IO ()
aiMode []  = putStrLn (paintStr " ---Empty dictionary---" red)
aiMode dictionary = do
    let border     = "+======================+"
    let emptySpace = "\n|                      |"

    putStrLn (paintStr border green)
    putStrLn (paintStr  "|       Guess mode     |" green)
    putStrLn (paintStr (border ++ emptySpace) green)
    putStrLn (paintStr ("|    Pick difficulty   |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (1) Easy         |" ++ emptySpace) green)
    putStrLn (paintStr ("|---> (2) Hard         |" ++ emptySpace) green)
    putStrLn (paintStr border green)

    mode <- getLine

    

    if mode == "1" then do
            putStrLn ("\n" ++ "--- WORDLE GAME STARTED: Easy difficulty ---")
            aiLoopNormal dictionary "xxxxx" 1

        else if mode == "2" 
            then do
            putStrLn ("\n" ++ "--- WORDLE GAME STARTED: Hard difficulty---")
            let expertDictionary = [ (word, 0) | word <- dictionary ]
            aiLoopExpert expertDictionary 1
            else do
                putStrLn (paintStr "\n==============================" red ++ "\n   --- Invalid mode! ---\n" ++ "==============================\n")
                aiMode dictionary
