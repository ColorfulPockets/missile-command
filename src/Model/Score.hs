{-# LANGUAGE RecordWildCards #-}
module Model.Score where

-- import Model.Board (Result (..), CellContents (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = Int 

init :: Int -> Score
init n = n

add :: Score -> Score
add sc = sc + 1

addVar :: Score -> Int -> Score
addVar sc i = sc + i
