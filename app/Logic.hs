module Logic where
import Data.List (sort, group)

-- ТИПОВЕ

-- Тип за ANSI цветови код (напр. "\ESC[32m")
type ColorCode = String
-- Основната структура режим игра - информация: (Буква, Позиция, Цвят)
type LetterInfo = (Char, Int, ColorCode)
-- Структура за режим игра "лесен": (Дума, Позиция)
type LetterPos = (Char, Int)
-- Структура за режим помощник "Експерт": (Дума, Брой натрупани грешки)
type ScoredWord = (String, Int)

-- Списък с всички букви от азбуката и началния им цвят (Reset)
alphabetList :: [(Char, ColorCode)]
alphabetList = zip ['a'..'z'] (repeat reset)

-- ANSI кодове за оцветяване на конзолата
green :: ColorCode
green  = "\ESC[32m"
yellow :: ColorCode
yellow = "\ESC[33m"
gray :: ColorCode
gray   = "\ESC[90m"
red :: ColorCode
red = "\ESC[31m"
reset :: ColorCode
reset  = "\ESC[0m"

-- Помощна функция, която оцветява даден низ със зададен цвят и връща Reset накрая
colorize :: [Char] -> ColorCode -> [Char]
colorize letter code = code ++ letter ++ reset

-- Превръща символ ('g', 'y', 'x') в съответния ANSI код
colorCode :: Char -> String
colorCode codeChar
    | codeChar == 'g' = green
    | codeChar == 'y' = yellow
    | otherwise       = gray

-- =============================================================================
-- ФИЛТРИРАНЕ НА РЕЧНИКА (ПО РАЗМЕР НА ДУМАТА)
-- =============================================================================

filterByWordLength :: [String] -> Int -> [String]
filterByWordLength [] _ = []
filterByWordLength (d:dictionary) len
    | length d == len  = d : filterByWordLength dictionary len
    | otherwise        = filterByWordLength dictionary len

-- =============================================================================
-- ОСНОВНА ЛОГИКА НА ИГРАТА (режим игра)
-- =============================================================================

-- Намира зелените букви и връща списък с намерените зелени тройки.
greenLetters :: (Eq a) => [a] -> [a] -> Int -> [(a, Int, ColorCode)]
greenLetters _ [] _ = []
greenLetters [] _ _ = []
greenLetters (x:xs) (y:ys) pos 
    | x == y          = (x, pos, green) : greenLetters xs ys (1 + pos)
    | otherwise       = greenLetters xs ys (1 + pos)

-- Намира жълтите букви и връща списък с намерените жълти тройки.
yellowLetters :: String -> String -> Int -> [LetterInfo]
yellowLetters [] _  _= []
yellowLetters (x:xs) word pos
    | x `elem` word && x /= '_'  =  (x, pos, yellow) : yellowLetters xs (rfm x word) (1 + pos)
    | otherwise       =  yellowLetters xs word (1 + pos)
        where
            rfm _ [] = []
            rfm s (z:zs)
                | s == z     = '_' : zs
                | otherwise  = z : rfm s zs

-- Намира сивите букви и връща списък с намерените сиви тройки.
grayLetters :: String -> Int -> [LetterInfo]
grayLetters [] _ = []
grayLetters (x:xs) pos 
    | x /= '_'    =  (x, pos, gray) : grayLetters xs (1 + pos)
    | otherwise   =  grayLetters xs (1 + pos)

-- Замества намерените зелени букви с '_' в даден низ
greenReplacer :: [Char] -> [LetterInfo] -> Int -> [Char]
greenReplacer [] _ _= []
greenReplacer x [] _= x
greenReplacer (x:xs) ((l,p,s):other) pos
    | x == l && p == pos  = '_' : greenReplacer xs other (pos + 1)
    | otherwise           = x : greenReplacer xs ((l,p,s):other) (pos + 1)

-- Замества намерените жълти букви с '_' в даден низ
yellowReplacer :: [Char] -> [LetterInfo] -> [Char]
yellowReplacer [] _= []
yellowReplacer x []= x
yellowReplacer lst ((l,_,_):other) = yellowReplacer (rfm l lst) other where
        rfm _ [] = []
        rfm s (z:zs)
            | s == z     = '_' : zs
            | otherwise  = z : rfm s zs

-- Приема предположение и тайна дума. Връща списък с LetterInfo за всяка буква.
tripleVec :: String -> String -> [LetterInfo]
tripleVec guess word = greens ++ yellows ++ grays 
    where
    greens  = greenLetters guess word 0
    yellows = yellowLetters withouthGreens (greenReplacer word greens 0) 0
    withouthGreens = greenReplacer guess greens 0
    withouthYellows = yellowReplacer withouthGreens yellows
    grays   = grayLetters withouthYellows 0

-- Сортира списъка с резултати по позиция
tripletsSorter :: [LetterInfo] -> [LetterInfo]
tripletsSorter [] = []
tripletsSorter ((x,pos,z):other) = tripletsSorter bigger ++ [(x,pos,z)] ++ tripletsSorter lower where
    lower  = [(s,p,t) | (s,p,t) <- other , p < pos]
    bigger = [(s,p,t) | (s,p,t) <- other , p >= pos]

-- Връща стринга на предположението, оцветен буква по буква според резултата.
letterPainter :: String -> String -> String
letterPainter guess word = foldr (\(x,_,z) acc -> acc ++ colorize [x] z) [] triplets where
    triplets = tripletsSorter (tripleVec guess word)

-- Взима дума от списък по индекс.
getWordByIndex :: [String] -> Int -> String
getWordByIndex [] _ = "ERROR"
getWordByIndex (x:_) 0 = x
getWordByIndex (_:xs) n =  getWordByIndex xs (n - 1)


-- Помощна функция за обновяване на цветовете на клавиатурата (Alphabet).
alphabetPainterHelper :: [(Char, ColorCode)] -> [(Char, ColorCode)] -> [(Char, ColorCode)]
alphabetPainterHelper _ [] = []
alphabetPainterHelper word ((x,y):letters)
    | y == green || y == gray     = (x,y) : alphabetPainterHelper word letters
    | otherwise                   = (x, newColor) : alphabetPainterHelper word letters
    where
        newColor = elemP x word y
        colorPriority :: ColorCode -> Int
        colorPriority colours
            | colours == green  = 3
            | colours == yellow = 2
            | colours == gray   = 1
            | otherwise         = 0
        elemP _ [] oldColour = oldColour
        elemP z ((l,c):guess) oldColour
            |z == l          = if colorPriority oldColour >= colorPriority c then elemP z guess oldColour else elemP z guess c
            |otherwise        = elemP z guess oldColour

-- Обновява клавиатурата след ход на играча.
alphabetPainter :: String -> String -> [(Char, String)] -> [(Char, ColorCode)]
alphabetPainter guess word = alphabetPainterHelper [(l,c) | (l,_,c) <- tripleVec guess word]

-- Оцветява целия стринг в един цвят
paintStr :: [Char] -> ColorCode -> [Char]
paintStr str colour = foldr (\x acc -> acc ++ colorize [x] colour) [] (reverse str)

-- Принтира клавиатурата на екрана
printAlphabet :: [(Char, ColorCode)] -> IO ()
printAlphabet pairs = do
    let stringList = map (\(c, color) -> color ++ [c] ++ reset) pairs
    putStrLn (unwords stringList)

-- =============================================================================
-- EASY MODE ЛОГИКА (режим игра)
-- =============================================================================

-- Проверява дали в азбуката има сива буква
containsGrayLetter :: String -> [(Char, ColorCode)] -> String
containsGrayLetter [] _ = ""
containsGrayLetter (x:guess) alphabet
    | isGray x alphabet = "There is a gray letter in your guess!"
    | otherwise          = containsGrayLetter guess alphabet where
        isGray _ [] = False
        isGray x ((y,ys):res)
            | x == y  && ys == gray  = True
            | otherwise              = isGray x res

containsYellowLetter :: String -> [(Char, ColorCode)] -> String
containsYellowLetter [] _ = ""
containsYellowLetter letters alphabet = containsAllY letters alphabetY where
    alphabetY = [char | (char,colour)<-alphabet, colour == yellow]
    containsAllY _ [] = ""
    containsAllY [] _ = "There is a missing yellow letter in your guess!"
    containsAllY (x:xs) ys
        | x `elem` ys  = containsAllY xs (deleteFirstEncoutner x ys)
        | otherwise    = containsAllY xs ys
    deleteFirstEncoutner _ [] = []
    deleteFirstEncoutner x (y:ys)
        | x == y    = ys
        | otherwise = y : deleteFirstEncoutner x ys

containsGreenLetter :: [LetterPos] -> [LetterPos] -> String
containsGreenLetter _ [] = ""
containsGreenLetter [] _ = "There is a mismatch in green letter positions!"
containsGreenLetter ((letter,pos):xs) ((gLetter,gPos):ys)
    | pos == gPos && letter/=gLetter  = "There is a mismatch in green letter positions!"
    |pos == gPos && letter == gLetter = containsGreenLetter xs ys
    | otherwise                       = containsGreenLetter xs ((gLetter,gPos):ys)


extractGreenPositions :: [LetterInfo] -> [LetterPos]
extractGreenPositions  triplets = [(l,p) | (l,p,c) <- triplets, c == green]

positionSort :: [LetterPos] -> [LetterPos]
positionSort [] = []
positionSort ((x,pos):other) = positionSort lower ++ [(x,pos)] ++ positionSort bigger where
    lower  = [(s,p) | (s,p) <- other , p < pos]
    bigger = [(s,p) | (s,p) <- other , p >= pos] 


-- =============================================================================
-- HARD MODE ЛОГИКА (режим игра)
-- =============================================================================

-- Проверява дали една потенциална лъжа е "добра" (т.е. не противоречи на историята).
isGoodLie :: Foldable t => t [LetterInfo] -> [LetterInfo] -> Bool
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

-- Оценява силата на патерна. Зеленото дава най-много точки.
getLieScore :: [LetterInfo] -> Int
getLieScore triplets = sum [points c | (_,_,c) <- triplets]
  where
    points color
        | color == green  = 3
        | color == yellow = 1
        | otherwise       = 0

-- AI е използван за направата на проверка за нулева стойност на резултата от generateLie
-- Генерира лъжлив патерн, ако AI-то реши да излъже.
generateLie :: Foldable t => t [LetterInfo] -> [String] -> String -> String -> Maybe [LetterInfo]
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
        


-- =============================================================================
-- ФИЛТРИРАНЕ НА РЕЧНИКА (режим помощник)
-- =============================================================================

-- Премахва думи, които нямат правилната буква на зелената позиция.
clearNotGreenLetters :: [String] -> [LetterInfo] -> [String]
clearNotGreenLetters [] _ = []
clearNotGreenLetters (d:dictionary) greens
    | matchAll    = d : clearNotGreenLetters dictionary greens
    | otherwise = clearNotGreenLetters dictionary greens
    where 
        matchAll = all (\(letter, position, _) -> d !! position == letter) greens

-- Премахва думи, които съдържат буква, маркирана като сива.
clearGrayLetters :: [String] -> [LetterInfo] -> [String]
clearGrayLetters [] _ = []
clearGrayLetters (d:dictionary) grays
    | hasBadLetter = clearGrayLetters dictionary grays
    | otherwise = d : clearGrayLetters dictionary grays
    where 
        hasBadLetter = any (\(letter, _, _) -> letter `elem` d) grays

-- Премахва думи, които нямат жълтите букви или ги имат точно на същата позиция.
clearNotYellowLetters :: [String] -> [LetterInfo] -> [String]
clearNotYellowLetters [] _ = []
clearNotYellowLetters (d:dictionary) yellows
    | matchAll    = d : clearNotYellowLetters dictionary yellows
    | otherwise = clearNotYellowLetters dictionary yellows
    where 
        matchAll = all (\(letter, position, _) -> letter `elem` d && d !! position /= letter) yellows

-- Главна функция за филтриране: комбинира логиката за Зелено, Жълто и Сиво.
filterColours :: [String] -> [LetterInfo] -> [String]
filterColours dictionary triples = 
    clearNotYellowLetters (clearNotGreenLetters (clearGrayLetters dictionary effectiveGrays) greens) yellows
    where
        greens  = [(l, p, c) | (l, p, c) <- triples, c == green]
        yellows = [(l, p, c) | (l, p, c) <- triples, c == yellow]
        rawGrays = [(l, p, c) | (l, p, c) <- triples, c == gray]

        safeChars = [l | (l, _, _) <- greens] ++ [l | (l, _, _) <- yellows]
        effectiveGrays = filter (\(l, _, _) -> not (l `elem` safeChars)) rawGrays


-- Генерира всички възможни цветови патерни за дадена дума спрямо целия речник.
allVariants :: String -> [String] -> [[ColorCode]]
allVariants d dictionary = [ [colour | (_,_,colour) <- tripletsSorter (tripleVec d word)] | word <- dictionary]

-- Брои колко често се среща всеки уникален патерн.
uniqueVariantsCount :: Eq a => [a] -> [(a, Int)]
uniqueVariantsCount [] = [] 
uniqueVariantsCount (s:strings) = (s , countDup) : uniqueVariantsCount removedDup where
    countDup   = 1 + length [ x | x <- strings , x == s]
    removedDup =        [ x | x <- strings , x /= s]

-- Изчислява резултат за думата. По-висок резултат значи, че думата разделя речника по-добре
scoreCount :: String -> [String] -> Int
scoreCount d dictionary = foldr (\(_,cnt) acc -> acc + cnt * (totalCount - cnt)) 0 variations where
    variations = uniqueVariantsCount (allVariants d dictionary) 
    totalCount = sum [ cnt | (_,cnt) <- variations ]


-- Помощна функция за парсване на вход от потребителя.
parserTriplets :: String -> String -> [LetterInfo]
parserTriplets guess pattern = [ (letter, pos, colorCode code) | (letter, code, pos) <- zip3 guess pattern [0..] ]

-- =============================================================================
-- HARD MODE ЛОГИКА (режим помощник)
-- =============================================================================

-- Обновява списъка с кандидати за Expert Mode.
updateCandidateWords :: [ScoredWord] -> String -> [ColorCode] -> [ScoredWord]
updateCandidateWords dictionary lastGuess userInput = 
    filter isStillPossibleWords (map checkEveryWord dictionary) 
    where
        checkEveryWord (word, errors) = (word, newErrors) where
            possiblePattern = reverse [colour | (_,_,colour) <- tripletsSorter (tripleVec lastGuess word)]
            newErrors = if userInput == possiblePattern then errors else errors + 1
        isStillPossibleWords (_, err) = err <= 1

-- Оптимизирана версия на броенето за AI Expert Mode.
uniqueVariantsCountAI :: Ord a => [a] -> [(a, Int)]
uniqueVariantsCountAI xs = map (\g -> (head g, length g)) (group (sort xs))


-- Превръща стринг от типа "gyx" в списък от ColorCodes
charsToColours :: String -> [ColorCode]
charsToColours [] = []
charsToColours (x:xs) 
    | x == 'g'  = green : charsToColours xs
    | x == 'y'  = yellow : charsToColours xs
    | otherwise = gray : charsToColours xs
        