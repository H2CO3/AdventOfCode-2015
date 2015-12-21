import qualified Data.Set as S
import Data.List
import Data.List.Split


type Replacement = (String, String)

parseInput :: String -> ([Replacement], String)
parseInput str = (repls, medicine)
	where
		parts = splitOn "\n\n" str
		medicine = filter (/= '\n') $ parts !! 1
		repls = map parseLine $ lines $ parts !! 0
		parseLine line = (sides !! 0, sides !! 1)
			where
				sides = splitOn " => " line

substringIndices :: String -> String -> [Int]
substringIndices str subs = findIndices (isPrefixOf subs) $ tails str

possibleReplacements :: String -> Replacement -> [String]
possibleReplacements str (orig, subs) = map replaceAt indices
	where
		indices = substringIndices str orig
		replaceAt :: Int -> String
		replaceAt n = take n str ++ subs ++ drop (n + length orig) str

allReplacements :: String -> [Replacement] -> [String]
allReplacements str repls = concatMap (possibleReplacements str) repls

main = do
	cont <- readFile "input.txt"
	let (repls, medicine) = parseInput cont
	-- Part 1
	print $ length $ S.fromList $ allReplacements medicine repls
