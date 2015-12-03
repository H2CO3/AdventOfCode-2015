import Data.List

main = do
	ps <- readFile("input.txt")
	let is = map (\x -> if x == '(' then 1 else -1) ps
	print $ sum is
	let firstBasement = elemIndex (-1 :: Int) $ scanl1 (+) is
	print $ maybe 0 id firstBasement + 1