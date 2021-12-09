{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (Result (..), CellContents (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scX    :: Int  -- ^ points for player X 
  , scO    :: Int  -- ^ points for player O 
  , scD    :: Int  -- ^ drawn games 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0 0

add :: Score -> Maybe CellContents -> Score
add sc (Just X) = sc { scX = scX sc + 1 }
add sc (Just (O _)) = sc { scO = scO sc + 1 }
add sc Nothing  = sc { scD = scD sc + 1 }
add sc _ = sc

addVar :: Score -> Maybe CellContents -> Int -> Score
addVar sc (Just X) i = sc { scX = scX sc + i }
addVar sc (Just (O _)) i = sc { scO = scO sc + i }
addVar sc Nothing i = sc { scD = scD sc + i }
addVar sc _  _ = sc

get :: Score -> CellContents -> Int
get Score {..} X = scX 
get Score {..} (O _) = scO 

currRound :: Score -> Int
currRound Score {..} = scX + scO + scD + 1

startPlayer :: Score -> CellContents
startPlayer sc 
  | even (currRound sc) = X
  | otherwise           = O ' '

winner :: Score -> Result () 
winner sc@Score {..}
  | scX > scO + left = Win X
  | scO > scX + left = Win (O ' ')
  | otherwise        = Cont ()
  where 
    left             = 1 + scMax - currRound sc