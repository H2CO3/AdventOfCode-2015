import Text.JSON
import Data.Ratio
import Data.List


sumAllNumbers :: JSValue -> Integer
sumAllNumbers (JSRational _ r) = numerator r
sumAllNumbers (JSArray vals) = sum $ map sumAllNumbers vals
sumAllNumbers (JSObject obj) = sum $ map (sumAllNumbers . snd) $ fromJSObject obj
sumAllNumbers _ = 0

sumWithoutRed :: JSValue -> Integer
sumWithoutRed (JSRational _ r) = numerator r
sumWithoutRed (JSArray vals) = sum $ map sumWithoutRed vals
sumWithoutRed (JSObject obj) =
	if any ((== (JSString $ toJSString "red")) . snd) $ fromJSObject obj then
		0 -- ignore
	else
		sum $ map (sumWithoutRed . snd) $ fromJSObject obj
sumWithoutRed _ = 0

nullizeParseError :: Result JSValue -> JSValue
nullizeParseError (Error _) = JSNull
nullizeParseError (Ok val) = val

main = do
	cont <- readFile "input.txt"
	let val = nullizeParseError $ decode cont
	print $ sumAllNumbers val
	print $ sumWithoutRed val
