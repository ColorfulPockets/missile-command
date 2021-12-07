module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import qualified Model.Score as Score
import Control.Monad.IO.Class (MonadIO(liftIO))
-- import Model.Player
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (progressBoard s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (progressBoard s)    
  T.VtyEvent (V.EvKey (V.KChar ' ') _) -> nextS s =<< liftIO (shoot s)              -- when space bar is clicked, a missile is shot
  T.VtyEvent (V.EvKey V.KUp   _)  -> nextS (move up s) =<< liftIO (progressBoard (s))
  T.VtyEvent (V.EvKey V.KDown _)  -> nextS (move down s) =<< liftIO (progressBoard (s))
  T.VtyEvent (V.EvKey V.KLeft _)  -> nextS (move left s) =<< liftIO (progressBoard (s))
  T.VtyEvent (V.EvKey V.KRight _) -> nextS (move right s) =<< liftIO (progressBoard (s))
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
shoot :: PlayState -> IO (Board)
-------------------------------------------------------------------------------
--shoot s = return (result (if changed then (updateScoreAndShoot s target) else ms))
shoot s = return (if changed then ((shootSurrounding s target)) else (ms))
  where
    target = psPos s
    b = psBoard s
    (ms, changed) = remove b target

--updateScoreAndShoot :: PlayState -> Pos -> Board
--updateScoreAndShoot s target = b
  --where
    --b = shootSurrounding s target
    ---_ = s { psScore = (Score.add (psScore s) (Just Model.Board.X)) }
    --sc' = (Score.add (psScore s) (Just X))
    --s'   = s { psScore = sc' }
    ---_ = Model.next s (Model.Board.UpdateScore b)


shootSurrounding :: PlayState -> Pos -> Board
shootSurrounding s (Pos i j) = b'''''
  where
    b = psBoard s
    (b'   , _) = remove b    (Pos (i - 1) j) -- up
    (b''  , _) = remove b'   (Pos (i + 1) j) -- down
    (b''' , _) = remove b''  (Pos i (j - 1)) -- left
    (b'''', _) = remove b''' (Pos i (j + 1)) -- right
    (b''''', _) = remove b'''' (Pos i j) -- TODO: this needs to be cleaned up

--shoot s = return (result (remove (psBoard s) (psPos s))) -- TODO: blast radius


getPos :: PlayState -> IO ([Pos], [Pos])
getPos s = do
  (p, del) <- travel (psBoard s)
  return (p, del)

-- This function controls how things on the board change.
-- If you want to change the board, start here.
-------------------------------------------------------------------------------
progressBoard :: PlayState -> IO (Board)
-------------------------------------------------------------------------------
progressBoard s = 
  putAndRemove2 (psBoard s) <$> getPos s -- this line moves all the misiles downward
  -- Add other lines here for anything in the board state that should change every tick (such as explosion animations)


-- TODO: This is where the score should be updated, I think
-------------------------------------------------------------------------------
nextS :: PlayState -> Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s (result b) of
  Right s' -> continue s'
  Left res -> halt (s) 


