import Data.Char (intToDigit, digitToInt)

runLengthEncode :: Int -> [Int] -> [[Int]]
runLengthEncode _ [] = []
runLengthEncode n (x:y:rest) =
	if x /= y then
		[n, x] : runLengthEncode 1 (y : rest)
	else
		runLengthEncode (n + 1) $ y : rest
runLengthEncode n (x:_) = [[n, x]]

lookAndSay = map intToDigit . concat . runLengthEncode 1 . map digitToInt

getSolution iters = length $ last $ take (iters + 1) $ iterate lookAndSay "1113222113"

main = (print $ getSolution 40) >> (print $ getSolution 50)
