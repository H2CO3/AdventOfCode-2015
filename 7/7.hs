import Data.List
import Data.List.Split
import Data.Word (Word16)
import Data.Bits
import Text.Read
import qualified Data.Map as M


data Node = Constant Word16
          | Wire String
          | Not Node
          | And Node Node
          | Or Node Node
          | Lshift Node Node
          | Rshift Node Node
          deriving (Show, Eq)

-- Map from wire names to connected 'AST' nodes
type Connections = M.Map String Node

-- Map from wire names to their actual signal values
type WireState = M.Map String Word16


parseLine :: String -> (String, Node)
parseLine s = (wireName, node)
	where
		sides = splitOn " -> " s
		wireName = sides !! 1
		parseTerm :: String -> Node
		parseTerm str = maybe (Wire str) Constant $ (readMaybe str :: Maybe Word16)
		node = case splitOn " " $ sides !! 0 of
			(l:"AND":r:_)    -> And (parseTerm l) (parseTerm r)
			(l:"OR":r:_)     -> Or (parseTerm l) (parseTerm r)
			(l:"LSHIFT":r:_) -> Lshift (parseTerm l) (parseTerm r)
			(l:"RSHIFT":r:_) -> Rshift (parseTerm l) (parseTerm r)
			("NOT":r:_)      -> Not $ parseTerm r
			(term:_)         -> parseTerm term

parseInput :: String -> Connections
parseInput = M.fromList . map parseLine . lines

findNamedConnection :: Connections -> String -> Node
findNamedConnection conns name =
	case M.lookup name conns of
		Just conn -> conn
		Nothing   -> error $ "cannot find wire named " ++ name

--
-- TODO: now this should really-really-REALLY be rewritten
-- using the State monad.
--

evaluate :: Node -> Connections -> WireState -> (Word16, WireState)
evaluate node conns state = case node of
	Constant value -> (value, state)
	Wire name      -> case M.lookup name state of
		-- if the name was found in the state, just return it
		Just v  -> (v, state)
		-- otherwise, try to find a connection that helps evaluating it,
		-- and add it to the state
		Nothing -> (v, newState2)
			where
				node = findNamedConnection conns name
				(v, newState1) = evaluate node conns state
				newState2 = M.insert name v newState1
	Not node       -> (complement value, newState)
		where
			(value, newState) = evaluate node conns state
	And lhs rhs    -> (lhsVal .&. rhsVal, newState2)
		where
			(lhsVal, newState1) = evaluate lhs conns state
			(rhsVal, newState2) = evaluate rhs conns newState1
	Or lhs rhs     -> (lhsVal .|. rhsVal, newState2)
		where
			(lhsVal, newState1) = evaluate lhs conns state
			(rhsVal, newState2) = evaluate rhs conns newState1
	Lshift lhs rhs -> (shiftL (fromIntegral lhsVal) (fromIntegral rhsVal), newState2)
		where
			(lhsVal, newState1) = evaluate lhs conns state
			(rhsVal, newState2) = evaluate rhs conns newState1
	Rshift lhs rhs -> (shiftR (fromIntegral lhsVal) (fromIntegral rhsVal), newState2)
		where
			(lhsVal, newState1) = evaluate lhs conns state
			(rhsVal, newState2) = evaluate rhs conns newState1

main = do
	cont <- readFile "input2.txt" -- or "input.txt"
	let conns = parseInput cont
	let (val, _) = evaluate (Wire "a") conns M.empty
	print val
