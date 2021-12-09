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
-- implicitly tested -- Bhavani
init :: Int -> Score
init n = n

-- tested -- Bhavani
-- Adds one point to score
add :: Score -> Score
add sc = sc + 1

-- tested -- Bhavani
-- Adds a variable amount to score
addVar :: Score -> Int -> Score
addVar sc i = sc + i
