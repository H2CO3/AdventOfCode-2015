import Data.Array.IO
import Data.List.Split
import Control.Monad


-- The array of lights: 2D Int -> Bool: On or Off
type Lights = IOUArray (Int, Int) Bool

-- ((x0, y0), (x1, y1)) represents the
-- rectangular area [x0..x1] x [y0...y1]
type RangeIndex = ((Int, Int), (Int, Int))

-- What to do with individual lights
data CommandKind = TurnOn | TurnOff | Toggle
     deriving (Show)

-- What to do with a range of lights
data Command = Command CommandKind RangeIndex
     deriving (Show)

-- The constant representing the entire array of lights
lightsRange :: RangeIndex
lightsRange = ((0, 0), (999, 999))

--
-- The functions below help parse each line of the input
--

splitCommand :: String -> (CommandKind, String)
splitCommand line =
	case words of
		("toggle":_)     -> (Toggle,  concatFrom 1 words)
		("turn":"on":_)  -> (TurnOn,  concatFrom 2 words)
		("turn":"off":_) -> (TurnOff, concatFrom 2 words)
		_                -> error "invalid command"
	where
		words = splitOn " " line
		concatFrom n xs = concat $ drop n xs

parseLine :: String -> Command
parseLine line = Command kind ((x0, y0), (x1, y1))
	where
		begend = splitOn "through" coords
		beg = begend !! 0
		end = begend !! 1
		xy0 = splitOn "," beg
		xy1 = splitOn "," end
		x0 = read (xy0 !! 0) :: Int
		y0 = read (xy0 !! 1) :: Int
		x1 = read (xy1 !! 0) :: Int
		y1 = read (xy1 !! 1) :: Int
		(kind, coords) = splitCommand line

--
-- Split input file in lines; parse each line into a command
--
parseFile :: String -> IO [Command]
parseFile fname = do
	cont <- readFile fname
	return $ map parseLine $ lines cont

-- Lights array factory

newLights :: RangeIndex -> Bool -> IO Lights
newLights = newArray

-- Perform the specified command on a range of lights,
-- where the range is given by an explicit list of indices.
writeElements :: Lights -> CommandKind -> [(Int, Int)] -> IO ()
writeElements _ _ [] = return ()
writeElements arr cmd (x:xs) = do
	e <- readArray arr x
	writeArray arr x $ nextValue e
	writeElements arr cmd xs
	where
		nextValue e = case cmd of
			Toggle  -> not e
			TurnOff -> False
			TurnOn  -> True

-- Perform the specified command on a range of lights,
-- where the range is given by its (inclusive) bounds, in the Command.
performCommand :: Lights -> Command -> IO ()
performCommand lights (Command cmd idx) = do
	let indices = range idx
	writeElements lights cmd indices

-- Apply a list of commands
performCommands :: Lights -> [Command] -> IO ()
performCommands = mapM_ . performCommand

-- Counts the number of lights that are turned on
countLights :: Lights -> IO Int
countLights lights = foldM add 0 =<< (mapM readBoolAsInt $ range lightsRange)
	where
		add :: Int -> Int -> IO Int
		add x y = return $ x + y

		readBoolAsInt :: (Int, Int) -> IO Int
		readBoolAsInt idx = do
			b <- readArray lights idx
			return $ if b then 1 else 0

main = do
	lights <- newLights lightsRange False
	commands <- parseFile "input.txt"
	-- print commands
	performCommands lights commands
	n <- countLights lights
	print n
