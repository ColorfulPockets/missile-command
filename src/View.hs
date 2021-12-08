module View (view) where

import Brick 
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import qualified Model.Score  as Score
import Model.Board
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    -- border $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..dim] ]

header :: PlayState -> String
header s = case ((psTypeCooldown s) == 0) of
  True  -> printf " Missile Command! Missiles Destroyed: = %s ---- Defenses: Active "
    (show (Score.get (psScore s) X)) 
  False -> printf " Missile Command! Missiles Destroyed: = %s ---- Defenses:   -    "
    (show (Score.get (psScore s) X)) 
  
          
            
            

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = raw --withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

withX :: Widget n -> Widget n
withX = modifyDefAttr (`withBackColor` red)

withO :: Widget n -> Widget n
withO = modifyDefAttr (`withBackColor` blue)

withF :: Widget n -> Widget n
withF = modifyDefAttr (`withBackColor` yellow)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = (mkCellContents xoMb)
  where 
    xoMb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkCellContents :: Maybe CellContents -> Widget n
mkCellContents (Just X) = blockX
mkCellContents (Just (O c)) = blockO c
mkCellContents (Just (F _ _ _)) = blockF
mkCellContents _  = blockB

blockB, blockX, blockF :: Widget n
-- blockB = vBox [fill ' ']
-- blockX = vBox [fill 'X']
-- blockO = vBox [fill 'O']
blockB = vBox [ str "  "]
blockX = withX (vBox [str " "])
blockF = withF (vBox [ str "  "])

blockO :: Char -> Widget n
blockO c = withO (vBox [str [c, c]])

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [b | b <- bs])
hTile _      = emptyWidget