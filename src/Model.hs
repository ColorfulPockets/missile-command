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
  , psMoveMissiles :: Int       -- Loops between 0 and ; missiles only move when it's 0
  , psTypeCooldown :: Int       -- Counts down the typing cooldown
  , psMissileCount :: Int       -- Keeps track of number of missiles currently on the board
  , prog           :: Int       -- Degree of randomness for spawning missiles
  , tickC          :: Int       -- Tick count to progress randomness
  } 

init :: PlayState
init = PS 
  { psScore  = Score.init 0
  , psBoard  = Board.init
  , psPos    = head (reverse Board.positions) 
  , psResult = Board.Cont ()
  , psMoveMissiles = 0
  , psTypeCooldown = 0
  , psMissileCount = 0 
  , prog           = 100
  , tickC          = 0
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s
  {
    psBoard = b',
    prog = if tickC s == 0 then max 3 (prog s - 1) else prog s,
    tickC = mod (tickC s + 1) 1000000
  }
  )

next s (Board.UpdateScore b') = Right (s { psBoard = b'
                                  , psScore = (Score.add (psScore s)) })
next _ _              = Left Board.Lose


