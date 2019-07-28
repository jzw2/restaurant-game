import qualified Data.Map.Lazy as M
import Data.Maybe
type Board = M.Map (Int, Int) Bool


createTriangle :: Int -> [Location]
createTriangle n =  concatMap f [0..n]
               where
                 f start = map (\x -> (start, x)) [0..start]
allLocations :: [(Int, Int)]
allLocations = createTriangle 4
initialBoard :: Board
initialBoard = M.insert (2, 1) False $ foldl (\acc elem -> M.insert elem True acc) M.empty allLocations 

type Location = (Int, Int)

data Move = Move Location Location deriving (Eq, Show)
-- src -> destination

midpoint :: Move -> Location
midpoint (Move (x1, y1) (x2, y2)) = (div (x1 + x2) 2, div (y1 + y2) 2)


allMoves :: [Move]
allMoves = concatMap moveFromTip $ createTriangle 2
  where
    moveFromTip (x, y) = [Move src dest | src <- corners, dest <- corners, src /= dest]
      where
        corners = [(x, y), (x + 2, y), (x + 2, y + 2)]


legalMoves :: Board -> [Move]
legalMoves b = filter legal allMoves
  where
    legal m@(Move src dest) = (b M.! src) && b M.! (midpoint m)  && not (b M.! dest)
    
applyMove :: Move -> Board -> Board
applyMove m@(Move src dest)  = f src . f dest . f (midpoint m)
  where
    f = M.adjust not

piecesLeft :: Board -> Int
piecesLeft  = length . filter id . M.elems 

solve :: (Board -> Bool) -> [Move] -> Board -> Maybe [Move]
solve winCondition moves b = if winCondition b
                      then Just moves
                      else case solvable of
                            [] -> Nothing
                            (x:_) -> x
                      where
                        l = legalMoves b
                        solvable = filter isJust $ map f $ l
                        f move = solve winCondition (moves ++ [move]) (applyMove move b)


solveCenter :: Maybe [Move]
solveCenter = solve w [] initialBoard
  where
    w b = b M.! (2, 1) && piecesLeft b == 1

solveAny :: Maybe [Move]
solveAny = solve (\b -> piecesLeft b == 1) [] initialBoard




-- getElement :: Board -> (Int, Int) -> Bool
-- getElement b (i, j) = b !! i !! j

-- (0,0)
-- (1, 0), (1, 1)
-- (2, 0), (2, 1), (2, 2)
-- (3, 0), (3, 1), (3, 2), (3, 3)
-- (4, 0), (4, 1), (4, 2), (4, 3), (4, 4)


