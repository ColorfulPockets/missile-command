module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import qualified Model.Score as Score
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
--shoot s = return (result (if changed then (updateScoreAndShoot s target) else ms))
shoot s = return (if changed then (Model.Board.UpdateScore (shootSurrounding s target)) else result(ms))
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

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = putAndRemove2 (psBoard s) xo <$> getPos xo s 
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO ([Pos], [Pos])
getPos xo s = do
  (p, del) <- getStrategy xo s (psPos s) (psBoard s) xo
  return (p, del)

getStrategy :: XO -> PlayState -> Strategy 
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 


