import Data.List

-- Old Rules

hasThreeVowels = (>= 3) . length . filter isVowel
	where
		isVowel :: Char -> Bool
		isVowel 'a' = True
		isVowel 'e' = True
		isVowel 'i' = True
		isVowel 'o' = True
		isVowel 'u' = True
		isVowel  _  = False

hasRepeat :: [Char] -> Bool
hasRepeat [] = False
hasRepeat [_] = False
hasRepeat (car:cadr:rest) = car == cadr || hasRepeat (cadr : rest)

hasNoProhibitedSubstring s = not $ or $ map (flip isInfixOf s) substrs
	where
		substrs = ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice s = hasThreeVowels s && hasRepeat s && hasNoProhibitedSubstring s

-- New Rules

hasRepeatingPair :: [Char] -> Bool
hasRepeatingPair [] = False
hasRepeatingPair [_, _] = False
hasRepeatingPair (car:cadr:rest) =
	isInfixOf [car, cadr] rest || hasRepeatingPair (cadr : rest)

hasInfixLetter :: [Char] -> Bool
hasInfixLetter []           = False
hasInfixLetter [_]          = False
hasInfixLetter [_, _]       = False
hasInfixLetter (x:y:z:rest) = x == z || hasInfixLetter (y : z : rest)

isNiceNew :: String -> Bool
isNiceNew s = hasRepeatingPair s && hasInfixLetter s

----------

countNice niceFn ls = length $ filter niceFn ls

main = do
	cont <- readFile "input.txt"
	let ls = lines cont
	-- The old rules
	print $ countNice isNice ls
	-- The new rules
	print $ countNice isNiceNew ls
