import Data.List.Split
import qualified Data.Map as M
import qualified Data.IntMap as IM


data Register = RA | RB | RIP deriving (Show, Eq, Ord)

data Instruction = Hlf Register
                 | Tpl Register
                 | Inc Register
                 | Jmp Int
                 | Jie Register Int
                 | Jio Register Int
                 deriving (Show)

type RegisterState = M.Map Register Int
type Program = IM.IntMap Instruction


--
-- Parsing the assembly code
--

parseLine :: String -> Instruction
parseLine line = case take 3 line of
	"hlf" -> Hlf $ regNamed args
	"tpl" -> Tpl $ regNamed args
	"inc" -> Inc $ regNamed args
	"jmp" -> Jmp $ readSigned args
	"jie" -> condJmp Jie
	"jio" -> condJmp Jio
	where
		args = drop 4 line
		jmpArgs = splitOn ", " args
		readSigned :: String -> Int
		readSigned = read . filter (/= '+')
		regNamed name = case name of
			"a" -> RA
			"b" -> RB
		condJmp ctor = ctor (regNamed $ jmpArgs !! 0) (readSigned $ jmpArgs !! 1)

parseInput :: String -> Program
parseInput = IM.fromList . (zip [0..]) . map parseLine . lines

--
-- Running instructions and programs
--

getReg :: RegisterState -> Register -> Int
getReg s r = maybe 0 id $ M.lookup r s

setReg :: RegisterState -> Register -> Int -> RegisterState
setReg s r v = M.insert r v s

transformReg :: Register -> (Int -> Int) -> RegisterState -> RegisterState
transformReg r t s = setReg s r $ t $ getReg s r

transformRegGotoNext r t s = transformReg RIP (+ 1) $ transformReg r t s
condJmp s cond offset =
	transformReg RIP (if cond then (+ offset) else (+ 1)) s

execInstr :: RegisterState -> Instruction -> RegisterState
execInstr s (Hlf reg)   = transformRegGotoNext reg (`div` 2) s
execInstr s (Tpl reg)   = transformRegGotoNext reg (* 3) s
execInstr s (Inc reg)   = transformRegGotoNext reg (+ 1) s
execInstr s (Jmp n)     = transformReg RIP (+ n) s
execInstr s (Jie reg n) = condJmp s (getReg s reg `mod` 2 == 0) n
execInstr s (Jio reg n) = condJmp s (getReg s reg == 1) n

-- if RIP goes out of bounds, we get back Nothing,
-- so we can use that as the termination condition.
-- Otherwise, we get back an instruction, so we execute
-- it on the state and recurse for the next instruction.
execProgram :: Program -> RegisterState -> RegisterState
execProgram prog s =
	maybe s (execProgram prog . execInstr s) $ IM.lookup (getReg s RIP) prog

main = do
	cont <- readFile "input.txt"
	let prog = parseInput cont
	-- Part 1
	print $ execProgram prog M.empty
	-- Part 2
	print $ execProgram prog $ M.singleton RA 1
