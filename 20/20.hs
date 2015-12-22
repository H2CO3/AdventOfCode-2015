import Data.List
import Math.NumberTheory.Factor
import Math.Combinat

-- https://programmers.stackexchange.com/q/264064#305595
sumDivisorsTimes10 =
	(10 *) . product . map (sum . scanl1 (*) . (1 :)) . group . pfactors

-- similar idea, but we can't play the "product of sums"
-- trick here, because here filtering is necessary.
sumDivisorsGTn50Times11 n =
	(11 *) $ sum $ filter (>= n `div` 50) $ map product $ listTensor $ map (scanl1 (*) . (1 :)) $ group $ pfactors n

-- 34000000 was my puzzle input
printSolution fn =
	print $ head $ filter ((>= 34000000) . snd) $ map (\n -> (n, fn n)) [1..]

main = do
	-- Part 1
	printSolution sumDivisorsTimes10
	-- Part 2
	printSolution sumDivisorsGTn50Times11
