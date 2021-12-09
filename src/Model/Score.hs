{-# LANGUAGE RecordWildCards #-}
module Model.Score where

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = Int 

-- Initializes a score
init :: Int -> Score
init n = n

-- tested -- Bhavani
-- Adds one point to score
add :: Score -> Score
add sc = sc + 1

-- Adds a variable amount to score
addVar :: Score -> Int -> Score
addVar sc i = sc + i
