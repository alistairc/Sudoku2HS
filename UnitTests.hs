module UnitTests (main) where

import qualified Data.Set as Set
import Data.Function ((&))

import MiniTest
import Main hiding (main)

sampleGridFull = Grid [
  Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9,
  Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 1, Just 2, Just 3,
  Just 7, Just 8, Just 9, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6,
  Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 1,
  Just 5, Just 6, Just 7, Just 8, Just 9, Just 1, Just 2, Just 3, Just 4,
  Just 8, Just 9, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7,
  Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 1, Just 2,
  Just 6, Just 7, Just 8, Just 9, Just 1, Just 2, Just 3, Just 4, Just 5,
  Just 9, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8
  ]

sampleGridPartial = Grid [
  Nothing, Just 2,  Just 3,  Nothing, Just 5,  Just 6,  Nothing, Just 8,  Just 9,
  Just 4,  Just 5,  Just 6,  Just 7,  Just 8,  Just 9,  Just 1,  Just 2,  Just 3,
  Just 7,  Just 8,  Just 9,  Just 1,  Just 2,  Just 3,  Just 4,  Just 5,  Just 6,
  Just 2,  Nothing, Just 4,  Just 5,  Nothing, Just 7,  Just 8,  Nothing, Just 1,
  Just 5,  Just 6,  Just 7,  Just 8,  Just 9,  Just 1,  Just 2,  Just 3,  Just 4,
  Just 8,  Just 9,  Just 1,  Just 2,  Just 3,  Just 4,  Just 5,  Just 6,  Just 7,
  Just 3,  Just 4,  Nothing, Just 6,  Just 7,  Nothing, Just 9,  Just 1,  Nothing,
  Just 6,  Just 7,  Just 8,  Just 9,  Just 1,  Just 2,  Just 3,  Just 4,  Just 5,
  Just 9,  Just 1,  Just 2,  Just 3,  Just 4,  Just 5,  Just 6,  Just 7,  Just 8
  ]

sampleGridSolve = Grid [
  Just 1,  Just 2,  Just 3,  Just 4,  Nothing, Just 6,  Just 7,  Just 8,  Just 9,
  Just 4,  Just 5,  Just 6,  Just 7,  Nothing, Just 9,  Just 1,  Just 2,  Just 3,
  Just 7,  Just 8,  Just 9,  Just 1,  Nothing, Just 3,  Just 4,  Just 5,  Just 6,
  Just 2,  Just 3,  Just 4,  Nothing, Nothing, Nothing, Just 8,  Just 9,  Just 1,
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, 
  Just 8,  Just 9,  Just 1,  Nothing, Nothing, Nothing, Just 5,  Just 6,  Just 7,
  Just 3,  Just 4,  Just 5,  Just 6,  Nothing, Just 8,  Just 9,  Just 1,  Just 2,
  Just 6,  Just 7,  Just 8,  Just 9,  Nothing, Just 2,  Just 3,  Just 4,  Just 5,
  Just 9,  Just 1,  Just 2,  Just 3,  Nothing, Just 5,  Just 6,  Just 7,  Just 8
  ]

emptyGrid = Grid $ replicate (gridSize * gridSize) Nothing

main = do
  let group = Group (Set.fromList [1,2])
  isComplete group `shouldBe` False

  let group2 = Group (Set.fromList [1..9])
  isComplete group2 `shouldBe` True

  Set.toList (missingDigits group) `shouldBe` [3,4,5,6,7,8,9]

  selectRow 1 sampleGridPartial `shouldBe` [Nothing, Just 2, Just 3, Nothing, Just 5, Just 6, Nothing, Just 8, Just 9]
  selectRow 2 sampleGridPartial `shouldBe` [Just 4, Just 5, Just 6, Just 7, Just 8, Just 9, Just 1, Just 2, Just 3]
  selectRow 10 sampleGridPartial `shouldBe` []

  selectCol 1 sampleGridPartial `shouldBe` [Nothing, Just 4, Just 7, Just 2, Just 5, Just 8, Just 3, Just 6, Just 9]
  selectCol 5 sampleGridPartial `shouldBe` [Just 5, Just 8, Just 2, Nothing, Just 9, Just 3, Just 7, Just 1, Just 4]

  selectSquare (1,1) sampleGridPartial `shouldBe` [
    Nothing, Just 2,  Just 3,
    Just 4,  Just 5,  Just 6,
    Just 7,  Just 8,  Just 9]
    
  selectSquare (3,1) sampleGridPartial `shouldBe` [
    Nothing, Just 8,  Just 9,
    Just 1,  Just 2,  Just 3,
    Just 4,  Just 5,  Just 6]

  selectSquare (1,3) sampleGridPartial `shouldBe` [
    Just 3,  Just 4,  Nothing,
    Just 6,  Just 7,  Just 8, 
    Just 9,  Just 1,  Just 2]

  (sampleGridPartial & selectSquare (1,3) & cellsToGroup & isComplete) `shouldBe` False
  (sampleGridPartial & selectSquare (1,3) & cellsToGroup & missingDigits) `shouldBe` Set.fromList [5]

  squareContaining (1,1) `shouldBe` (1,1)
  squareContaining (9,9) `shouldBe` (3,3)
  squareContaining (2,2) `shouldBe` (1,1)
  squareContaining (1,4) `shouldBe` (1,2)

  possibleDigits sampleGridSolve (1,1) `shouldBe` Set.fromList []
  possibleDigits sampleGridSolve (5,2) `shouldBe` Set.fromList [8]
  possibleDigits sampleGridSolve (5,5) `shouldBe` Set.fromList [1,2,3,4,5,6,7,8,9]
  possibleDigits sampleGridSolve (4,5) `shouldBe` Set.fromList [2,5,8]

  cellAt (2,1) sampleGridPartial `shouldBe` Just 2
  cellAt (2,4) sampleGridPartial `shouldBe` Nothing

  -- corners
  cellAt (1,1) sampleGridPartial `shouldBe` Nothing
  cellAt (9,1) sampleGridPartial `shouldBe` Just 9
  cellAt (1,9) sampleGridPartial `shouldBe` Just 9
  cellAt (9,9) sampleGridPartial `shouldBe` Just 8


  let newGrid = gridWith (1,1) Nothing sampleGridFull
    in do
      cellAt (1,1) newGrid `shouldBe` Nothing
      cellAt (2,1) newGrid `shouldBe` Just 2

  let newGrid = gridWith (3,3) (Just 1) sampleGridPartial
    in do
      cellAt (3,3) newGrid `shouldBe` Just 1

  -- already solved
  cellAt (2,2) sampleGridSolve `shouldBe` Just 5
  solveCell (2,2) sampleGridSolve `shouldBe` Just 5

  cellAt (1,4) sampleGridSolve `shouldBe` Just 2
  solveCell (1,4) sampleGridSolve `shouldBe` Just 2

  solveCell (5,5) sampleGridSolve `shouldBe` Nothing   -- not solvable 
  solveCell (1,5) sampleGridSolve `shouldBe` Just 5    -- easy
  solveCell (4,6) sampleGridSolve `shouldBe` Just 2    -- harder  

  solveIter sampleGridPartial `shouldBe` sampleGridFull
  solveIter sampleGridSolve `shouldBe` sampleGridFull

  formatGrid sampleGridSolve `shouldBe`
    "1234 6789\n\
    \4567 9123\n\
    \7891 3456\n\
    \234   891\n\
    \         \n\
    \891   567\n\
    \3456 8912\n\
    \6789 2345\n\
    \9123 5678\n"

  parseGrid "" `shouldBe` emptyGrid
  parseGrid
    "1234 6789\n\
    \4567 9123\n\
    \7891 3456\n\
    \234   891\n\
    \         \n\
    \891   567\n\
    \3456 8912\n\
    \6789 2345\n\
    \9123 5678\n"
    `shouldBe` sampleGridSolve
