{-# LANGUAGE RecordWildCards #-}
module Model.Score (
    Score,
    Model.Score.init,
    add,
    addVar
) where

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = Int 

-- Initializes a score
init :: Int -> Score
init n = n

-- tested
-- Adds one point to score
add :: Score -> Score
add sc = sc + 1

-- tested
-- Adds a variable amount to score
addVar :: Score -> Int -> Score
addVar sc i = sc + i
