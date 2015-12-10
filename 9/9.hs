import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split


type DistMatrix = M.Map (String, String) Int

parseDistMatrix :: String -> DistMatrix
parseDistMatrix = M.fromList . concatMap parseLine . lines
	where
		parseLine :: String -> [((String, String), Int)]
		parseLine line = [((fromName, toName), dist), ((toName, fromName), dist)]
			where
				sides = splitOn " = " line
				names = splitOn " to " $ sides !! 0
				dist = read $ sides !! 1 :: Int
				fromName = names !! 0
				toName = names !! 1

-- Unique all the names!
getTownList :: String -> [String]
getTownList = S.toList . S.fromList . concatMap parseLine . lines
	where
		parseLine = splitOn " to " . (!! 0) . splitOn " = "


--
-- There are only a few towns - when in doubt, use brute force!
-- Generate all permutations; compute total distance for each route
--

routeDistance :: DistMatrix -> [String] -> Int
routeDistance dists (car:cadr:rest) =
	townDistance car cadr + routeDistance dists (cadr : rest)
	where
		townDistance a b = maybe 0 id $ M.lookup (a, b) dists
routeDistance _ _ = 0

extremumRouteDistance :: ([Int] -> Int) -> DistMatrix -> [[String]] -> Int
extremumRouteDistance extremum dists routes =
	extremum $ map (routeDistance dists) routes

main = do
	cont <- readFile "input.txt"
	let dists = parseDistMatrix cont
	let names = getTownList cont
	let allRoutes = permutations names
	print $ extremumRouteDistance minimum dists allRoutes
	print $ extremumRouteDistance maximum dists allRoutes
