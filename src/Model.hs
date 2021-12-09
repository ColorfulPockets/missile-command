{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result
  , prog     :: Int     
  , psMoveMissiles :: Int       -- Loops between 0 and ; missiles only move when it's 0
  , psTypeCooldown :: Int       -- Counts down the typing cooldown
  , psMissileCount :: Int       -- Keeps track of number of missiles currently on the board
  } 

init :: PlayState
init = PS 
  { psScore  = Score.init 0
  , psBoard  = Board.init
  , psPos    = head (reverse Board.positions) 
  , psResult = Board.Cont ()
  , prog     = 100
  , psMoveMissiles = 0
  , psTypeCooldown = 0
  , psMissileCount = 0 
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b', prog = (max 5 ((prog s) - 1))
                                  })
next s (Board.UpdateScore b') = Right (s { psBoard = b'
                                  , psScore = (Score.add (psScore s)) })
next _ _              = Left Board.Lose


