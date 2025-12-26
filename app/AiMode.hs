module AiMode where
import Logic
import System.Random (randomRIO)

clearNotGreenLetters :: [String] -> [(Char, Int, String)] -> [String]
clearNotGreenLetters [] _ = []
clearNotGreenLetters (d:dictionary) greens
    | matchAll    = d : clearNotGreenLetters dictionary greens
    | otherwise = clearNotGreenLetters dictionary greens
    where 
        matchAll = all (\(letter, position, _) -> d !! position == letter) greens

clearGrayLetters :: [String] -> [(Char, Int, String)] -> [String]
clearGrayLetters [] _ = []
clearGrayLetters (d:dictionary) grays
    | hasBadLetter = clearGrayLetters dictionary grays
    | otherwise = d : clearGrayLetters dictionary grays
    where 
        hasBadLetter = any (\(letter, _, _) -> letter `elem` d) grays

clearNotYellowLetters :: [String] -> [(Char, Int, String)] -> [String]
clearNotYellowLetters [] _ = []
clearNotYellowLetters (d:dictionary) yellows
    | matchAll    = d : clearNotYellowLetters dictionary yellows
    | otherwise = clearNotYellowLetters dictionary yellows
    where 
        matchAll = all (\(letter, position, _) -> letter `elem` d && d !! position /= letter) yellows
filterColours :: [String] -> [(Char, Int, String)] -> [String]
filterColours dictionary triples = 
    clearNotYellowLetters (clearNotGreenLetters (clearGrayLetters dictionary effectiveGrays) greens) yellows
    where
        greens  = [(l, p, c) | (l, p, c) <- triples, c == green]
        yellows = [(l, p, c) | (l, p, c) <- triples, c == yellow]
        
        -- Всички "сиви" от входа
        rawGrays = [(l, p, c) | (l, p, c) <- triples, c == gray]

        -- Списък с букви, които знаем, че СЪЩЕСТВУВАТ (зелени или жълти)
        safeChars = [l | (l, _, _) <- greens] ++ [l | (l, _, _) <- yellows]

        -- Филтрираме сивите: Оставяме само тези, които НЕ са в списъка safeChars.
        -- Така второто 'O' ще бъде изхвърлено от сивия списък и няма да изтрие думата "POWER".
        effectiveGrays = filter (\(l, _, _) -> not (l `elem` safeChars)) rawGrays
-- tripletsSorter (tripleVec guess word)

allVariants :: String -> [String] -> [[String]]
allVariants d dictionary = [ [colour | (_,_,colour) <- tripletsSorter (tripleVec d word)] | word <- dictionary]

uniqueVariantsCount :: Eq a => [a] -> [(a, Int)]
uniqueVariantsCount [] = [] 
uniqueVariantsCount (s:strings) = (s , countDup) : uniqueVariantsCount removedDup where
    countDup   = 1 + length [ x | x <- strings , x == s]
    removedDup =        [ x | x <- strings , x /= s]


scoreCount :: String -> [String] -> Int
scoreCount d dictionary = foldr (\(_,cnt) acc -> acc + cnt * (totalCount - cnt)) 0 variations where
    variations = uniqueVariantsCount (allVariants d dictionary) 
    totalCount = sum [ cnt | (_,cnt) <- variations ]

colorCode :: Char -> String
colorCode codeChar
    | codeChar == 'g' = green
    | codeChar == 'y' = yellow
    | otherwise       = gray

parserTriplets :: String -> String -> [(Char, Int, String)]
parserTriplets guess pattern = [ (letter, pos, colorCode code) | (letter, code, pos) <- zip3 guess pattern [0..] ]

bestWord :: [String] -> String -> Int -> IO ()
bestWord [] _ _=  putStrLn (paintStr ("\n==============================" ++ "\n--- No possible words left! ---\n" ++ "==============================\n") red)
bestWord dictionary allwrong atempt = do
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

                    bestWord newDictionary colourLine (atempt + 1)
        else do
            let scores = [ (word, scoreCount word dictionary) | word <- dictionary ]
            let maxScore = maximum [ score | (_, score) <- scores ]
            let bestWords = [ word | (word, score) <- scores , score == maxScore ]
            let chosenWord = head bestWords

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
                    
                    bestWord newDictionary colourLine (atempt + 1)


aiLoop :: [String] -> IO ()
aiLoop []  = putStrLn (paintStr " ---Empty dictionary---" red)
aiLoop dictionary = do
    putStrLn (paintStr "\n--- AI MODE STARTED ---\n" yellow)
    bestWord dictionary "xxxxx" 1
