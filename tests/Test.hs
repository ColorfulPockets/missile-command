{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)
import Model.Board

main :: IO ()
main = runTests 
  [boardTests]

boardTests ::  Score -> TestTree
boardTests sc = testGroup "Board module"
  [ scoreTest (down, (Pos 1 1),  (Pos 2 1)                      , 2, "down")
  , scoreTest (notNone, (F 1 1 DirUp),  True                    , 2, "notNone-1")
  , scoreTest (notNone, (O 'k'),  True                          , 2, "notNone-2")
  , scoreTest (notNone, X,  True                                , 2, "notNone-3")
  , scoreTest (notNone, None,  False                            , 2, "notNone-3")
  , scoreTest (remove Model.Board.init,  (Pos 1 1), (Model.Board.init, None), 2, "remove-1")
  , scoreTest (remove b10,  (Pos 1 1), (b10r, (F 2 2 DirDown))  , 2, "remove-2")
  , scoreTest (remove b10,  (Pos 2 2), (b10r2, (O 'L'))         , 2, "remove-3")
  , scoreTest (remove (gameOverBoard 0),  (Pos 2 2), ((gameOverBoard 0), (None)) , 2, "remove-4")
  , scoreTest (remove (gameOverBoard 1),  (Pos 20 20), ((gameOverBoard 1), (None)) , 2, "remove-5")
  , scoreTest (bottomRowHasMissile,  bBottomRow, True , 2, "bottomRowHasMissile-1")
  , scoreTest (bottomRowHasMissile,  b10, False       , 2, "bottomRowHasMissile-2")
  , scoreTest (explodeAround expPos, b10, bExploded   , 2, "explodeAround1")
  , scoreTest (explodeAround expPos2, b10, bExploded2 , 2, "explodeAround2")
  , scoreTest (getMissilesMinusTopRow, b10Top, misInB10r, 2, "getMissilesMinusTopRow1")
  , scoreTest (getMissilesMinusTopRow, b10, misInB10r, 2, "getMissilesMinusTopRow2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
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
