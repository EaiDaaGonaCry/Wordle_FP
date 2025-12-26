module Logic where
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

--- Here starts the logic for GAME MODE --*

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


-- Hard Mode Logic

isGoodLie :: Foldable t => t [(Char, Int, String)] -> [(Char, Int, String)] -> Bool
isGoodLie historyOfWords lieCandidate = all checkForOne historyOfWords where
    checkForOne oldTriplets = yellowContradiction && greenContradiction && grayContradiction
        where
            greenContradiction = null [(nChar,nPos,nColour) | 
                (oChar,oPos,oColour) <- oldTriplets, 
                (nChar , nPos, nColour) <- lieCandidate,
                    oChar == nChar,
                    oColour == green,
                    nColour /= green,
                    oPos == nPos]
            grayContradiction = null [(nChar,nPos,nColour) | 
                (oChar,_,oColour) <- oldTriplets, 
                (nChar , nPos, nColour) <- lieCandidate,
                    oChar == nChar,
                    oColour == gray,
                    nColour /= gray]
            yellowContradiction = null [(nChar,nPos,nColour) | 
                (oChar,oPos,oColour) <- oldTriplets,
                (nChar , nPos, nColour) <- lieCandidate,
                    oChar == nChar,
                    oColour == yellow,
                    nColour == gray || (nColour == green && oPos /= nPos)]

getLieScore :: [(Char, Int, String)] -> Int
getLieScore triplets = sum [points c | (_,_,c) <- triplets]
  where
    points color
        | color == green  = 3
        | color == yellow = 1
        | otherwise       = 0

-- AI е използван за направата на проверка за нулева стойност на резултата от generateLie
generateLie :: Foldable t => t [(Char, Int, String)] -> [String] -> String -> String -> Maybe [(Char, Int, String)]
generateLie historyOfWords dictionary currentGuess secretWord =
    case idealCandidates of
        (best:_) -> Just best 
        []       -> case allCandidates of
                        (fallback:_) -> Just fallback
                        []           -> Nothing       
    where
        allCandidates = [ lieCandidate | lieWord <- dictionary,
                          let lieCandidate = tripletsSorter (tripleVec currentGuess lieWord),    
                          lieWord /= secretWord,
                          lieWord /= currentGuess, 
                          isGoodLie historyOfWords lieCandidate ] 
        idealCandidates = [ c | c <- allCandidates, 
                            let s = getLieScore c, 
                            s >= 4 && s <= 9 ]
        


--- Here starts the logic for AI MODE --*


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