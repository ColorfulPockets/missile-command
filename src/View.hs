module View (view) where

import Brick 
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

-- tested -- Andrew
view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel ((str (header)) <+> (status)) $
      vTile [ mkRow s row | row <- [1..dim] ]
        where 
          header = printf " Missile Command! Missiles Destroyed: = %s ---- Defenses: "
            (show (psScore s)) 
          status = case ((psTypeCooldown s) == 0) of
            True  -> (blackForeground (greenBackground (str " Active "))) <+> (str "  ")
            False -> (blackForeground (withX (str "   --   "))) <+> (str "  ")

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withX :: Widget n -> Widget n
withX = modifyDefAttr (`withBackColor` red)

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
mkCell' s r c = (mkCellContents xoMb)
  where 
    xoMb      = psBoard s ! Pos r c

mkCellContents :: Maybe CellContents -> Widget n
mkCellContents (Just X) = blockX
mkCellContents (Just (O c)) = blockO c
mkCellContents (Just (F _ _ _)) = blockF
mkCellContents _  = blockB

blockB, blockX, blockF :: Widget n
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

