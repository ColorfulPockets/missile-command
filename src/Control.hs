module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
-- import qualified Data.Map as M 

import Model
import Model.Board
import qualified Model.Score as Score
import Control.Monad.IO.Class (MonadIO(liftIO))
-- import Model.Player
-- import Model.Player 

import Data.Char
-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (progressBoard s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (progressBoard s)    
--  T.VtyEvent (V.EvKey (V.KChar ' ') _) -> nextS s =<< liftIO (shoot s)              -- when space bar is clicked, a missile is shot
  -- T.VtyEvent (V.EvKey V.KUp   _)  -> nextS (move up s) =<< liftIO (progressBoard (s))
  -- T.VtyEvent (V.EvKey V.KDown _)  -> nextS (move down s) =<< liftIO (progressBoard (s))
  -- T.VtyEvent (V.EvKey V.KLeft _)  -> nextS (move left s) =<< liftIO (progressBoard (s))
  -- T.VtyEvent (V.EvKey V.KRight _) -> nextS (move right s) =<< liftIO (progressBoard (s))
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  T.VtyEvent (V.EvKey (V.KChar c) _) -> nextS s =<< liftIO (shootChar s (toLower c))              -- when a certain letter is clicked, that missile is shot
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
--move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
--move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
shootChar :: PlayState -> Char -> IO (Board)
-------------------------------------------------------------------------------
shootChar s c = shoot s target
  where
    posList = (Model.Board.findCharPos (psBoard s) c) -- gets the position mapped to that character
    target = case posList of 
                    [] -> psPos s     -- TODO: decide what should happen when the letter they typed is not associated with any missile
                    (x:_) -> x

-------------------------------------------------------------------------------
shoot :: PlayState -> Pos -> IO (Board)
-------------------------------------------------------------------------------
--shoot s = return (result (if changed then (updateScoreAndShoot s target) else ms))
shoot s target = return (if changed then ((shootSurrounding (psBoard s) target)) else (ms))
  where
    b = psBoard s
    (ms, c) = remove b target
    changed = notNone c

--updateScoreAndShoot :: PlayState -> Pos -> Board
--updateScoreAndShoot s target = b
  --where
    --b = shootSurrounding s target
    ---_ = s { psScore = (Score.add (psScore s) (Just Model.Board.X)) }
    --sc' = (Score.add (psScore s) (Just X))
    --s'   = s { psScore = sc' }
    ---_ = Model.next s (Model.Board.UpdateScore b)

getPos :: PlayState -> IO ([(Pos, CellContents)], [(Pos, CellContents)])
getPos s = do
  (p, del) <- travel (psBoard s) n
  return (p, del)
    where
      n = prog s -- TODO FIX PROGRESSION

-- This function controls how things on the board change.
-- If you want to change the board, start here.
-------------------------------------------------------------------------------
progressBoard :: PlayState -> IO (Board)
-------------------------------------------------------------------------------
progressBoard s = case psMoveMissiles s of
    0 -> do
      b <- putAndRemove2 (psBoard s) <$> getPos s-- this line moves all the misiles downward
      return (moveExplosions b)
    _ -> return (moveExplosions (psBoard s))
  -- Add other lines here for anything in the board state that should change every tick (such as explosion animations)


-- TODO: This is where the score should be updated, I think
-------------------------------------------------------------------------------
nextS :: PlayState -> Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s (result b) of
  Right s' -> case missileCounter of
    1 -> continue s' {psMoveMissiles = 0}
    _ -> continue s' {psMoveMissiles = missileCounter + 1}
    where
      missileCounter = psMoveMissiles s' 
  Left res -> halt (s) 


