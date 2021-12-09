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
    scoreTest (getFs, b7List, b7Expected, 2, "getFs-4")
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