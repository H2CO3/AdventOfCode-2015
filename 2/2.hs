import Data.List.Split

main = do
	-- parse file
	c <- readFile "input.txt"
	let dims = map ((map (read :: String -> Int)) . (splitOn "x")) $ lines c
	-- compute areas
	print $ pairSum $ map computeAreaAndLength dims
	where
		pairSum xys = (sum . map get1 $ xys, sum . map get2 $ xys)
		get1 (x, _) = x
		get2 (_, y) = y
		computeAreaAndLength ls = (
			2 * (a1 + a2 + a3) + minimum [a1, a2, a3],
			a * b * c + 2 * minimum [l1, l2, l3])
			where
				a = ls !! 0
				b = ls !! 1
				c = ls !! 2
				a1 = a * b
				a2 = a * c
				a3 = b * c
				l1 = a + b
				l2 = a + c
				l3 = b + c
