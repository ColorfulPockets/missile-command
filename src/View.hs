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
    borderWithLabel ((str (header)) <+> (status)) $
      vTile [ mkRow s row | row <- [1..dim] ]
        where 
          header = printf " Missile Command! Missiles Destroyed: = %s ---- Defenses: "
            (show (Score.get (psScore s) X)) 
          status = case ((psTypeCooldown s) == 0) of
            True  -> (blackForeground (greenBackground (str " Active "))) <+> (str "  ")
            False -> (blackForeground (withX (str "   --   "))) <+> (str "  ")

  
          
            
            

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = raw --withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

-- withCursor :: Widget n -> Widget n
-- withCursor = modifyDefAttr (`withStyle` reverseVideo)

withX :: Widget n -> Widget n
withX = modifyDefAttr (`withBackColor` red)

withO :: Widget n -> Widget n
withO = modifyDefAttr (`withBackColor` blue)

withF :: Widget n -> Widget n
withF = modifyDefAttr (`withBackColor` yellow)

greenBackground :: Widget n -> Widget n
greenBackground = modifyDefAttr (`withBackColor` green)

blackForeground :: Widget n -> Widget n
blackForeground = modifyDefAttr (`withForeColor` black)

whiteForeground :: Widget n -> Widget n
whiteForeground = modifyDefAttr (`withForeColor` white)

blackBackground :: Widget n -> Widget n
blackBackground = modifyDefAttr (`withBackColor` black)

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
blockB = blackBackground (vBox [ str "  "])
blockX = withX (vBox [str "  "])
blockF = withF (vBox [ str "  "])

blockO :: Char -> Widget n
blockO c = blackBackground (whiteForeground (vBox [str [c, ' ']]))

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [b | b <- bs])
hTile _      = emptyWidget

-----------------------------------------------------------------------------

