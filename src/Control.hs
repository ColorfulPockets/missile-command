module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)    
  T.VtyEvent (V.EvKey (V.KChar ' ') _) -> nextS s =<< liftIO (shoot s)              -- when space bar is clicked, a missile is shot
  T.VtyEvent (V.EvKey V.KUp   _)  -> nextS (move up s) =<< liftIO (play O (s))
  T.VtyEvent (V.EvKey V.KDown _)  -> nextS (move down s) =<< liftIO (play O (s))
  T.VtyEvent (V.EvKey V.KLeft _)  -> nextS (move left s) =<< liftIO (play O (s))
  T.VtyEvent (V.EvKey V.KRight _) -> nextS (move right s) =<< liftIO (play O (s))
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
shoot :: PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
shoot s = return (result (remove (psBoard s) (psPos s)))

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = putAndRemove (psBoard s) xo <$> getPos xo s 
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO (Pos, Pos)
getPos xo s = do
  (p, del) <- getStrategy xo s (psPos s) (psBoard s) xo
  case del of
    Nothing -> return (p, Pos 0 0)
    Just x  -> return (p, x)

getStrategy :: XO -> PlayState -> Strategy 
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 


