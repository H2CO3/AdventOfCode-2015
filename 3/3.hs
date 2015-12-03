import qualified Data.Map as M
import qualified Data.Set as S

data Coord = Coord { x :: Integer,
                     y :: Integer
                   }
                   deriving(Show, Eq, Ord)

type CoordMap = M.Map Coord Integer

type SantaState = (Coord, CoordMap)

origin :: Coord
origin = Coord { x = 0, y = 0 }

(|+|) :: Coord -> Coord -> Coord
(|+|) a b = Coord { x = x a + x b,
                    y = y a + y b
                  }

step :: Char -> Coord
step '^' = Coord { x =  0, y =  1 }
step 'v' = Coord { x =  0, y = -1 }
step '<' = Coord { x = -1, y =  0 }
step '>' = Coord { x =  1, y =  0 }
step  _  = error "invalid character"

-- TODO: rewrite this using State
countHouses :: [Char] -> SantaState -> SantaState
countHouses [] s = s
countHouses (x:xs) (c, m) = (nextCoord, M.insert nextCoord (val + 1) nextMap)
	where
		nextCoord = c |+| step x
		val = maybe 0 id $ M.lookup nextCoord m
		(_, nextMap) = countHouses xs (nextCoord, m)


twoNPlusZero :: [a] -> [a]
twoNPlusZero [] = []
twoNPlusZero (car:cadr:rest) = car : twoNPlusZero rest
twoNPlusZero (car:rest) = car : twoNPlusZero rest

twoNPlusOne :: [a] -> [a]
twoNPlusOne [] = []
twoNPlusOne (car:cadr:rest) = cadr : twoNPlusOne rest
twoNPlusOne (car:rest) = [] ++ twoNPlusZero rest

mergeMaps :: CoordMap -> CoordMap -> S.Set Coord
mergeMaps m1 m2 = S.union (M.keysSet m1) (M.keysSet m2)

main = do
	steps <- readFile "input.txt"
	let (_, m) = countHouses steps (origin, startMap)
	print $ M.size m
	let (santa, robot) = (twoNPlusZero steps, twoNPlusOne steps)
	let (_, s) = countHouses santa (origin, startMap)
	let (_, r) = countHouses robot (origin, startMap)
	print $ S.size $ mergeMaps s r
		where
			startMap = M.singleton origin 1
