--
-- Part 1
--
unescape :: String -> String
unescape s = unescape' $ drop 1 $ take ((length s) - 1) s
	where
		unescape' []                  = []
		unescape' ('\\':'\\':xs)      = '\\' : unescape' xs
		unescape' ('\\':'"':xs)       = '"'  : unescape' xs
		unescape' ('\\':'x':hi:lo:xs) = '?'  : unescape' xs
		unescape' (x:xs)              =  x   : unescape' xs

--
-- Part 2
--
escape :: String -> String
escape s = "\"" ++ escape' s ++ "\""
	where
		escape' []        = []
		escape' ('\\':xs) = "\\\\" ++ escape' xs
		escape' ('"':xs)  = "\\\"" ++ escape' xs
		escape' (x:xs)    = x       : escape' xs


main = do
	cont <- readFile "input.txt"
	let orig = lines cont
	let unesc = map unescape orig
	let esc = map escape orig
	let origLen = sum $ map length orig
	let unescLen = sum $ map length unesc
	let escLen = sum $ map length esc
	print $ origLen - unescLen
	print $ escLen - origLen
