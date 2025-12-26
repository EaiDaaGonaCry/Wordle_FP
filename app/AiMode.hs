module AiMode where
import Logic
import System.Random (randomRIO)

aiLoop :: [String] -> String -> Int -> IO ()
aiLoop [] _ _=  putStrLn (paintStr ("\n==============================" ++ "\n--- No possible words left! ---\n" ++ "==============================\n") red)
aiLoop dictionary allwrong atempt = do
    putStrLn (paintStr ("\n--- Possible words left: " ++ show (length dictionary) ++ " ---\n") red)
    if allwrong == "xxxxx" 
        then do
            let wordCount = length dictionary
            rnd <- randomRIO (0, wordCount - 1)
            let guessWord = getWordByIndex dictionary rnd

            putStrLn (paintStr "==================================" yellow)
            putStrLn (paintStr ("---Attempt number " ++ show atempt ++ ": ---") yellow)
            putStrLn (paintStr ("---Is your word: " ++ guessWord ++ " ? (y/n)---") yellow)
            putStrLn (paintStr "==================================" yellow)
            
            response <- getLine
            if response == "y"
                then putStrLn (paintStr ("\n==============================" ++ "\n      --- AI WIN! ---\n" ++ "==============================\n") green)
                else do
                    putStrLn "Enter the pattern (g - green, y - yellow, x- gray): "
                    colourLine <- getLine
                    let triples = parserTriplets guessWord colourLine

                    let filteredDict = filterColours dictionary triples
                    let newDictionary = filter (/= guessWord) filteredDict

                    aiLoop newDictionary colourLine (atempt + 1)
        else do
            let scores = [ (word, scoreCount word dictionary) | word <- dictionary ]
            let maxScore = maximum [ score | (_, score) <- scores ]
            let aiLoops = [ word | (word, score) <- scores , score == maxScore ]
            let chosenWord = head aiLoops

            putStrLn (paintStr "==================================" yellow)
            putStrLn (paintStr ("---Attempt number " ++ show atempt ++ ": ---") yellow)
            putStrLn (paintStr ("---Is your word: " ++ chosenWord ++ " ? (y/n)---") yellow)
            putStrLn (paintStr "==================================" yellow)
            
            response <- getLine
            if response == "y"
                then putStrLn (paintStr ("\n==============================" ++ "\n      --- AI WIN! ---\n" ++ "==============================\n") green)
                else do
                    putStrLn "Enter the pattern (g - green, y - yellow, x- gray): "
                    colourLine <- getLine
                    let triples = parserTriplets chosenWord colourLine

                    let filteredDict = filterColours dictionary triples
                    let newDictionary = filter (/= chosenWord) filteredDict
                    
                    aiLoop newDictionary colourLine (atempt + 1)


aiMode :: [String] -> IO ()
aiMode []  = putStrLn (paintStr " ---Empty dictionary---" red)
aiMode dictionary = do
    putStrLn (paintStr "\n--- AI MODE STARTED ---\n" yellow)
    aiLoop dictionary "xxxxx" 1
