import Data.Char
import qualified Data.Set as S

nextWord = reverse . nextWord' . reverse
	where
		nextWord' :: String -> String
		nextWord' [] = "b"
		nextWord' ('z':xs) = 'a' : nextWord' xs
		nextWord' (x:xs) = (chr $ ord x + 1) : xs

hasStraight :: String -> Bool
hasStraight (x:y:z:rest) = xn + 1 == yn && yn + 1 == zn || hasStraight (y : z : rest)
	where
		xn = ord x
		yn = ord y
		zn = ord z
hasStraight _ = False

notConfusing :: String -> Bool
notConfusing = not . or . (`map` "iol") . flip elem

hasTwoPairs :: String -> Bool
hasTwoPairs = (>= 2) . S.size . S.fromList . letterPairs
	where
		letterPairs (x:y:rest) =
			if x == y then
				x : letterPairs rest
			else
				letterPairs (y : rest)
		letterPairs _ = []

isValid :: String -> Bool
isValid s = hasStraight s && notConfusing s && hasTwoPairs s

nextValid = nextWord . last . takeWhile (not . isValid) . tail . iterate nextWord

-- input was "hepxcrrq"
main = do
	print $ nextValid "hepxcrrq"
	print $ nextValid "hepxxyzz"
