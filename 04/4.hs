import Data.Digest.MD5 (hash)
import Codec.Utils
import Data.Char
import Data.List (findIndex)

showHexByte :: Int -> [Char]
showHexByte b = [hi, lo]
	where
		hiNibble = b `div` 16
		loNibble = b `mod` 16
		hi = ds !! hiNibble
		lo = ds !! loNibble
		ds = "0123456789abcdef"

octetToHex :: Codec.Utils.Octet -> [Char]
octetToHex = showHexByte . fromIntegral

digestToHex = concatMap octetToHex

stringMD5 = digestToHex . hash . map (fromIntegral . ord :: Char -> Codec.Utils.Octet)

-- "ckczppom" was my input
firstNOfHashes n x = map (take n . stringMD5 . ("ckczppom" ++) . show) x

indexOfFirstStartingWithNZeroes n =
	findIndex (== nZeros) $ firstNOfHashes n [0..]
	where
		nZeros = take n $ repeat '0'

main = do
	print $ indexOfFirstStartingWithNZeroes 5
	print $ indexOfFirstStartingWithNZeroes 6
