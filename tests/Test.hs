{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import BoardVars
import Prelude hiding (maximum)
import Model.Board
import Model.Score as GameScore

main :: IO ()
main = runTests 
  [
    boardTests,
    scoreTests
  ]

boardTests ::  Common.Score -> TestTree
boardTests sc = testGroup "Board Module"
  [ scoreTest (down, (Pos 5 1), (Pos 6 1), 2, "down"),
    scoreTest (up, (Pos 5 1), (Pos 4 1), 2, "up"),
    scoreTest (left, (Pos 1 5), (Pos 1 4), 2, "left"),
    scoreTest (right, (Pos 1 5), (Pos 1 6), 2, "right"),
    scoreTest ((\_ -> notIn emptyBoard 0 1), 0, True, 2, "notIn-1"),
    scoreTest ((\_ -> notIn b1 1 1), 0, False, 2, "notIn-2"),
    scoreTest (result, b2, (Lose), 2, "result-1"),
    scoreTest (result, b3, (Lose), 2, "result-2"),
    scoreTest (result, b1, (Cont b1), 2, "result-3"),
    scoreTest (getFs, b4List, b4Expected, 2, "getFs-1"),
    scoreTest (getFs, b5List, b5Expected, 2, "getFs-2"),
    scoreTest (getFs, b6List, b6Expected, 2, "getFs-3"),
    scoreTest (getFs, b7List, b7Expected, 2, "getFs-4"),
    scoreTest (notNone, (Fire 1 1 DirUp), True, 2, "notNone-1"), 
    scoreTest (notNone, (Ms 'k'), True, 2, "notNone-2"),
    scoreTest (notNone, X, True, 2, "notNone-3"),
    scoreTest (notNone, None, False, 2, "notNone-3"),
    scoreTest (remove Model.Board.init, (Pos 1 1), (Model.Board.init, None), 2, "remove-1"),
    scoreTest (remove b10, (Pos 1 1), (b10r, (Fire 2 2 DirDown)), 2, "remove-2"),
    scoreTest (remove b10, (Pos 2 2), (b10r2, (Ms 'L')), 2, "remove-3"),
    scoreTest (remove (gameOverBoard 0), (Pos 2 2), ((gameOverBoard 0), (None)), 2, "remove-4"),
    scoreTest (remove (gameOverBoard 1), (Pos 20 20), ((gameOverBoard 1), (None)), 2, "remove-5"),
    scoreTest (bottomRowHasMissile, bBottomRow, True, 2, "bottomRowHasMissile-1"),
    scoreTest (bottomRowHasMissile, b10, False, 2, "bottomRowHasMissile-2"),
    scoreTest (explodeAround expPos, b10, bExploded, 2, "explodeAround1"),
    scoreTest (explodeAround expPos2, b10, bExploded2, 2, "explodeAround2"),
    scoreTest (getMissilesMinusTopRow, b10Top, misInB10r, 2, "getMissilesMinusTopRow1"),
    scoreTest (getMissilesMinusTopRow, b10, misInB10r, 2, "getMissilesMinusTopRow2"),


    scoreTest (putAndRemove simpleBoard, sbShift, sbResult, 2, "simple-move"),
    scoreTest (putAndRemove advancedBoard, advShift, advResult, 2, "advanced-move"),

    scoreTest (isMissile, msX, False, 1, "check-x"),
    scoreTest (isMissile, msO, True, 1, "check-o"),
    scoreTest (isMissile, msF, False, 1, "check-f"),
    scoreTest (isMissile, msN, False, 1, "check-n"),

    scoreTest (posWithCellContents sbPos, simpleBoard, sbOld, 2, "simple-pos-cc"),
    scoreTest (posWithCellContents advPos, advancedBoard, advOld, 2, "advanced-pos-cc"),
    scoreTest (posWithCellContents pwccPos, pwccBoard, pwccContents, 2, "all-pos-cc"),

    scoreTest (getMissiles, simpleBoard, sbPos, 2, "simple-missiles"),
    scoreTest (getMissiles, advancedBoard, advPos, 2, "advanced-missiles"),
    scoreTest (getMissiles, pwccBoard, pwccMissiles, 2, "all-missiles")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
    emptyBoard = Model.Board.init 
    b1 = put emptyBoard (Ms 'A') (Pos 1 1)    --has an O in the board
    b2 = put emptyBoard (Ms 'A') (Pos dim 3)  --has an O in bottomRow
    b3 = put emptyBoard (X) (Pos (dim-1) 4)  --has an X in mostPos
    b4List = [((Pos (dim-1) 4), (Fire 0 0 DirDown)), ((Pos 3 3), (Ms 'B')), ((Pos 32 7), (Ms 'C')), ((Pos 12 11), (Fire 0 0 DirDown))]
    b4Expected = [(Pos (dim-1) 4), (Pos 12 11)]
    b5List = [((Pos (dim-1) 4), (Ms 'A')), ((Pos 3 3), (Ms 'B')), ((Pos 32 7), (Ms 'C')), ((Pos 12 11), (X))]
    b5Expected = []
    b6List = []
    b6Expected = []
    b7List = [((Pos (dim-1) 4), (Fire 0 0 DirDown)), ((Pos 3 3), (Fire 0 0 DirDown)), ((Pos 32 7), (Fire 0 0 DirDown)), ((Pos 12 11), (Fire 0 0 DirDown))]
    b7Expected = [(Pos (dim-1) 4), (Pos 3 3), (Pos 32 7), (Pos 12 11)]
    b10 = put (put Model.Board.init (Fire 2 2 DirDown) (Pos 1 1)) (Ms 'L') (Pos 2 2)
    b10r = put Model.Board.init (Ms 'L') (Pos 2 2)
    b10r2 = put Model.Board.init (Fire 2 2 DirDown) (Pos 1 1)
    b10Top = put b10 (Ms 'R') (Pos 1 24)
    misInB10r = [(Pos 2 2)]
    bBottomRow = put b10 (Ms 'o') (Pos dim dim)
    bExploded = put (put (put
      (put (put b10 (Fire 1 10 DirUp) (up expPos)) (Fire 1 10 DirLeft) (left expPos))
      (Fire 1 10 DirDown) (down expPos))
      (Fire 1 10 DirRight) (right expPos))
      (Fire 10 10 DirUp) expPos
    bExploded2 = put (put (put
      (put (put b10 (Fire 1 10 DirUp) (up expPos2)) (Fire 1 10 DirLeft) (left expPos2))
      (Fire 1 10 DirDown) (down expPos2))
      (Fire 1 10 DirRight) (right expPos2))
      (Fire 10 10 DirUp) expPos2
    expPos = (Pos 25 25)
    expPos2 = (Pos 25 1)

scoreTests ::  Common.Score -> TestTree
scoreTests sc = testGroup "Score Module"
  [ scoreTest (add, sco, 101, 2, "add"),
    scoreTest ((\_ -> addVar sco 10), 0, 110, 2, "addVar-1"),
    scoreTest ((\_ -> addVar sco (-4)), 0, 96, 2, "addVar-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
    sco = GameScore.init 100
