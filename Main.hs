module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function ((&))
import Data.Maybe

main = do
  print "hello"

type Cell = Maybe Int
newtype Group = Group (Set Int)
newtype Grid = Grid [Cell]

squareSize = 3
gridSize = squareSize * squareSize

isComplete :: Group -> Bool
isComplete group =
  let (Group digits) = group in
  Set.size digits == gridSize

missingDigits group =
  let (Group digits) = group in
  Set.difference allDigits digits

cellsToGroup :: [Cell] -> Group
cellsToGroup cells = Group (cells & catMaybes & Set.fromList)

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

cellIndex (x, y) = (y - 1) * gridSize + (x - 1)

allCellCoords =  (,) <$> [1..gridSize] <*> [1..gridSize] 


-- nextSolution :: Grid -> Grid
-- nextSolution grid = 
--   Grid $ map (\coord -> newCell $ cellAt coord grid) allCellCoords
--   where 
--     newCell currentCell
--   go (1,1) grid
--   where
--     go (x,y) grid = 
--       let possible = possibleDigits grid (x,y) & Set.toList
--        in case possible of
--          [single] -> gridWith (x,y) single