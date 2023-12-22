module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function ((&))
import Data.Maybe

main = do
  print "hello"

type Cell = Maybe Int
newtype Group = Group (Set Int)
newtype Grid = Grid [Cell] deriving (Show, Eq)

squareSize = 3
gridSize = squareSize * squareSize

isComplete :: Group -> Bool
isComplete group =
  let (Group digits) = group in
  Set.size digits == gridSize

missingDigits :: Group -> Set Int
missingDigits group =
  let (Group digits) = group in
  Set.difference allDigits digits

cellsToGroup :: [Cell] -> Group
cellsToGroup cells = Group (cells & catMaybes & Set.fromList)

allDigits :: Set Int
allDigits = Set.fromList [1..gridSize]

selectRow :: Int -> Grid -> [Cell]
selectRow rowNum (Grid cells) =
  cells
  & drop ((rowNum - 1) * gridSize)
  & take gridSize

selectCol :: Int -> Grid -> [Cell]
selectCol colNum (Grid cells) =
  cells
  & splitEvery gridSize
  & map (\row -> row & drop (colNum - 1) & head)  -- this line isn't very readable

selectSquare :: (Int, Int) -> Grid -> [Cell]
selectSquare (x,y) (Grid cells) =
  let
    rowsToSkip = (y - 1) * squareSize
    colsToSkip = (x - 1) * squareSize
  in
    cells
    & splitEvery gridSize
    & drop rowsToSkip & take squareSize
    & map (\row -> row & drop colsToSkip & take squareSize)
    & concat

squareContaining :: (Int, Int) -> (Int, Int)
squareContaining (x,y) =
  (((x -1) `div` squareSize) + 1, ((y - 1) `div` squareSize) + 1)

possibleDigits :: Grid -> (Int, Int) -> Set Int
possibleDigits grid (x, y) =
  let
    squarePos = squareContaining (x,y)
    colGroup = cellsToGroup $ selectCol x grid
    rowGroup = cellsToGroup $ selectRow y grid
    squareGroup = cellsToGroup $ selectSquare squarePos grid
  in
    foldr Set.intersection allDigits [missingDigits colGroup, missingDigits rowGroup, missingDigits squareGroup]

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)


gridWith :: (Int, Int) -> Cell -> Grid -> Grid
gridWith coords cell (Grid cells) =
  let pos = cellIndex coords
   in Grid (take pos cells ++ [cell] ++ drop (pos + 1) cells)

cellAt :: (Int, Int) -> Grid -> Cell
cellAt coords (Grid cells) =
  cells !! cellIndex coords

cellIndex :: (Int, Int) -> Int
cellIndex (x, y) = (y - 1) * gridSize + (x - 1)

allCellCoords :: [(Int, Int)]
allCellCoords =  (,) <$> [1..gridSize] <*> [1..gridSize]

solveCell :: (Int, Int) -> Grid -> Maybe Int
solveCell (x,y) grid =
  current & whenNothing trySolve
  where
    current = cellAt (x,y) grid
    trySolve = if Set.size possibles == 1 then Set.toList possibles & head & Just else Nothing
    possibles = possibleDigits grid (x,y)
    whenNothing nothingVal source = maybe source Just nothingVal

  
nextSolution :: Grid -> Grid
nextSolution initial = allCellCoords & foldr (\coord grid -> gridWith coord (solveCell coord initial) grid) initial

solveIter :: Grid -> Grid
solveIter current = 
  let next = nextSolution current
   in if next == current then current else solveIter next

formatGrid :: Grid -> String
formatGrid (Grid cells) =
  cells & map cellToChar & splitEvery gridSize & unlines

cellToChar :: Cell -> Char
cellToChar cell = maybe ' ' (head . show) cell
