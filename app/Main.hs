module Main where
import System.Random (randomRIO)

-- AI
alphabetList :: [(Char, String)]
alphabetList = zip ['a'..'z'] (repeat reset)

green :: String
green  = "\ESC[32m"
yellow :: String
yellow = "\ESC[33m"
gray :: String
gray   = "\ESC[90m"
red :: String
red = "\ESC[31m"
reset :: String
reset  = "\ESC[0m"

colorize :: [Char] -> [Char] -> [Char]
colorize letter code = code ++ letter ++ reset

greenLetters :: (Eq a) => [a] -> [a] -> Int -> [(a, Int, String)]
greenLetters _ [] _ = []
greenLetters [] _ _ = []
greenLetters (x:xs) (y:ys) pos 
    | x == y          = (x, pos, green) : greenLetters xs ys (1 + pos)
    | otherwise       = greenLetters xs ys (1 + pos)

yellowLetters :: String -> String -> Int -> [(Char, Int, String)]
yellowLetters [] _  _= []
yellowLetters (x:xs) word pos
    | x `elem` word && x /= '_'  =  (x, pos, yellow) : yellowLetters xs (rfm x word) (1 + pos)
    | otherwise       =  yellowLetters xs word (1 + pos)
        where
            rfm _ [] = []
            rfm s (z:zs)
                | s == z     = '_' : zs
                | otherwise  = z : rfm s zs

grayLetters :: String -> Int -> [(Char, Int, String)]
grayLetters [] _ = []
grayLetters (x:xs) pos 
    | x /= '_'    =  (x, pos, gray) : grayLetters xs (1 + pos)
    | otherwise   =  grayLetters xs (1 + pos)

greenReplacer :: [Char] -> [(Char, Int, c)] -> Int -> [Char]
greenReplacer [] _ _= []
greenReplacer x [] _= x
greenReplacer (x:xs) ((l,p,s):other) pos
    | x == l && p == pos  = '_' : greenReplacer xs other (pos + 1)
    | otherwise           = x : greenReplacer xs ((l,p,s):other) (pos + 1)


yellowReplacer :: [Char] -> [(Char, Int, c)] -> [Char]
yellowReplacer [] _= []
yellowReplacer x []= x
yellowReplacer lst ((l,_,_):other) = yellowReplacer (rfm l lst) other where
        rfm _ [] = []
        rfm s (z:zs)
            | s == z     = '_' : zs
            | otherwise  = z : rfm s zs
tripleVec :: String -> String -> [(Char, Int, String)]
tripleVec guess word = greens ++ yellows ++ grays 
    where
    greens  = greenLetters guess word 0
    yellows = yellowLetters withouthGreens (greenReplacer word greens 0) 0
    withouthGreens = greenReplacer guess greens 0
    withouthYellows = yellowReplacer withouthGreens yellows
    grays   = grayLetters withouthYellows 0

tripletsSorter :: [(Char, Int, String)] -> [(Char, Int, String)]
tripletsSorter [] = []
tripletsSorter ((x,pos,z):other) = tripletsSorter bigger ++ [(x,pos,z)] ++ tripletsSorter lower where
    lower  = [(s,p,t) | (s,p,t) <- other , p < pos]
    bigger = [(s,p,t) | (s,p,t) <- other , p >= pos]

letterPainter :: String -> String -> String
letterPainter guess word = foldr (\(x,_,z) acc -> acc ++ colorize [x] z) [] triplets where
    triplets = tripletsSorter (tripleVec guess word)

getWordByIndex :: [String] -> Int -> String
getWordByIndex [] _ = "ERROR"
getWordByIndex (x:_) 0 = x
getWordByIndex (_:xs) n =  getWordByIndex xs (n - 1)

alphabetPainterHelper :: [(Char, String)] -> [(Char, String)] -> [(Char, String)]
alphabetPainterHelper _ [] = []
alphabetPainterHelper word ((x,y):letters)
    | y == green || y == gray     = (x,y) : alphabetPainterHelper word letters
    | otherwise                   = (x, newColor) : alphabetPainterHelper word letters
    where
        newColor = elemP x word y
        colorPriority colours
            | colours == green  = 3
            | colours == yellow = 2
            | colours == gray   = 1
            | otherwise         = 0
        elemP _ [] oldColour = oldColour
        elemP z ((l,c):guess) oldColour
            |z == l          = if colorPriority oldColour >= colorPriority c then elemP z guess oldColour else elemP z guess c
            |otherwise        = elemP z guess oldColour

alphabetPainter :: String -> String -> [(Char, String)] -> [(Char, String)]
alphabetPainter guess word = alphabetPainterHelper [(l,c) | (l,_,c) <- tripleVec guess word]

paintStr :: [Char] -> [Char] -> [Char]
paintStr str colour = foldr (\x acc -> acc ++ colorize [x] colour) [] (reverse str)

--Ai
printAlphabet :: [(Char, String)] -> IO ()
printAlphabet pairs = do
    let stringList = map (\(c, color) -> color ++ [c] ++ reset) pairs
    putStrLn (unwords stringList)

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
        else if not (guess `elem` validWords)
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

-- Function to print a rainbow string
main :: IO ()
main = do

    content <- readFile "valid-wordle-words.txt"
    let allWords = lines content
    let wordCount = length allWords

    if wordCount == 0 
        then putStrLn "The file is empty!"
        else do
            randomIndex <- randomRIO (0, wordCount - 1)
            let secretWord = getWordByIndex allWords randomIndex
            
        --putStrLn ("Picked random index: " ++ show randomIndex)
        --putStrLn ("The secret word is: " ++ secretWord)

            putStrLn ("\n" ++ "--- WORDLE GAME STARTED ---")
            gameLoop secretWord allWords alphabetList 6

