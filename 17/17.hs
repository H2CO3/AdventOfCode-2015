import Control.Monad
import Data.Generics

parseInput = map (read :: String -> Int) . lines

powerset = filterM $ const constrs
powersetSums = map (\s -> (s, sum $ s)) . powerset
powersetSumsEqualing s n = map fst $ filter ((== n) . snd) $ powersetSums s

main = do
	cont <- readFile "input.txt"
	let containers = parseInput cont
	let subsets = powersetSumsEqualing containers 150
	-- Part 1
	print $ length subsets
	-- Part 2
	let minLength = minimum $ map length subsets
	print $ length $ filter ((== minLength) . length) subsets
