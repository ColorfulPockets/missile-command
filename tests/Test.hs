{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)
import Model.Board
import Model.Score as GameScore

main :: IO ()
main = runTests 
  [ boardTests,
    scoreTests
  ]

boardTests ::  Common.Score -> TestTree
boardTests sc = testGroup "Board module"
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
    scoreTest (notNone, (F 1 1 DirUp), True, 2, "notNone-1"), 
    scoreTest (notNone, (O 'k'), True, 2, "notNone-2"),
    scoreTest (notNone, X, True, 2, "notNone-3"),
    scoreTest (notNone, None, False, 2, "notNone-3"),
    scoreTest (remove Model.Board.init, (Pos 1 1), (Model.Board.init, None), 2, "remove-1"),
    scoreTest (remove b10, (Pos 1 1), (b10r, (F 2 2 DirDown)), 2, "remove-2"),
    scoreTest (remove b10, (Pos 2 2), (b10r2, (O 'L')), 2, "remove-3"),
    scoreTest (remove (gameOverBoard 0), (Pos 2 2), ((gameOverBoard 0), (None)), 2, "remove-4"),
    scoreTest (remove (gameOverBoard 1), (Pos 20 20), ((gameOverBoard 1), (None)), 2, "remove-5"),
    scoreTest (bottomRowHasMissile, bBottomRow, True, 2, "bottomRowHasMissile-1"),
    scoreTest (bottomRowHasMissile, b10, False, 2, "bottomRowHasMissile-2"),
    scoreTest (explodeAround expPos, b10, bExploded, 2, "explodeAround1"),
    scoreTest (explodeAround expPos2, b10, bExploded2, 2, "explodeAround2"),
    scoreTest (getMissilesMinusTopRow, b10Top, misInB10r, 2, "getMissilesMinusTopRow1"),
    scoreTest (getMissilesMinusTopRow, b10, misInB10r, 2, "getMissilesMinusTopRow2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
    emptyBoard = Model.Board.init 
    b1 = put emptyBoard (O 'A') (Pos 1 1)    --has an O in the board
    b2 = put emptyBoard (O 'A') (Pos dim 3)  --has an O in bottomRow
    b3 = put emptyBoard (X) (Pos (dim-1) 4)  --has an X in mostPos
    b4List = [((Pos (dim-1) 4), (F 0 0 DirDown)), ((Pos 3 3), (O 'B')), ((Pos 32 7), (O 'C')), ((Pos 12 11), (F 0 0 DirDown))]
    b4Expected = [(Pos (dim-1) 4), (Pos 12 11)]
    b5List = [((Pos (dim-1) 4), (O 'A')), ((Pos 3 3), (O 'B')), ((Pos 32 7), (O 'C')), ((Pos 12 11), (X))]
    b5Expected = []
    b6List = []
    b6Expected = []
    b7List = [((Pos (dim-1) 4), (F 0 0 DirDown)), ((Pos 3 3), (F 0 0 DirDown)), ((Pos 32 7), (F 0 0 DirDown)), ((Pos 12 11), (F 0 0 DirDown))]
    b7Expected = [(Pos (dim-1) 4), (Pos 3 3), (Pos 32 7), (Pos 12 11)]
    b10 = put (put Model.Board.init (F 2 2 DirDown) (Pos 1 1)) (O 'L') (Pos 2 2)
    b10r = put Model.Board.init (O 'L') (Pos 2 2)
    b10r2 = put Model.Board.init (F 2 2 DirDown) (Pos 1 1)
    b10Top = put b10 (O 'R') (Pos 1 24)
    misInB10r = [(Pos 2 2)]
    bBottomRow = put b10 (O 'o') (Pos dim dim)
    bExploded = put (put (put
      (put (put b10 (F 1 10 DirUp) (up expPos)) (F 1 10 DirLeft) (left expPos))
      (F 1 10 DirDown) (down expPos))
      (F 1 10 DirRight) (right expPos))
      (F 10 10 DirUp) expPos
    bExploded2 = put (put (put
      (put (put b10 (F 1 10 DirUp) (up expPos2)) (F 1 10 DirLeft) (left expPos2))
      (F 1 10 DirDown) (down expPos2))
      (F 1 10 DirRight) (right expPos2))
      (F 10 10 DirUp) expPos2
    expPos = (Pos 25 25)
    expPos2 = (Pos 25 1)

scoreTests ::  Common.Score -> TestTree
scoreTests sc = testGroup "Board module"
  [ scoreTest (add, sco, 101, 2, "add"),
    scoreTest ((\_ -> addVar sco 10), 0, 110, 2, "addVar-1"),
    scoreTest ((\_ -> addVar sco (-4)), 0, 96, 2, "addVar-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
    sco = GameScore.init 100
