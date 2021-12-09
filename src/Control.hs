module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import qualified Model.Score as Score
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Char
-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (progressBoard s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (progressBoard s)    
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  T.VtyEvent (V.EvKey (V.KChar c) _) -> case psTypeCooldown s of
    0 -> nextS (s {psTypeCooldown = cooldownLength}) =<< liftIO (shootChar s (toUpper c))    -- when a certain letter is clicked, that missile is shot
    _ -> nextS s =<< liftIO (progressBoard s)
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
processScoreNext :: (PlayState, Board) -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
processScoreNext (s', b') = nextS s' b' 


-- number of ticks between typing at max speed
cooldownLength :: Int
cooldownLength = 15

-------------------------------------------------------------------------------
shootChar :: PlayState -> Char -> IO (Board)
-------------------------------------------------------------------------------
shootChar s c = shoot s target
  where
    posList = (Model.Board.findCharPos (psBoard s) c)
    target = case posList of 
                    [] -> psPos s
                    (x:_) -> x

-------------------------------------------------------------------------------
shoot :: PlayState -> Pos -> IO (Board)
-------------------------------------------------------------------------------
shoot s target = return (if changed then (newBoard) else (ms))
  where
    b = psBoard s
    (ms, c) = remove b target
    changed = notNone c
    newBoard = (shootSurrounding (psBoard s) target)

getPos :: PlayState -> IO ([(Pos, CellContents)], [(Pos, CellContents)])
getPos s = do
  (p, del) <- travel (psBoard s) n
  return (p, del)
    where
      n = prog s 

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

-------------------------------------------------------------------------------
nextS :: PlayState -> Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s (result b) of
  Right s' -> case missileCounter of
    1 -> continue updateScore {psMoveMissiles = 0, psMissileCount = numMissiles}
    _ -> continue updateScore {psMoveMissiles = missileCounter + 1, psMissileCount = numMissiles}
    where
      missileCounter = psMoveMissiles s' 
      numMissiles = length (getMissiles (psBoard s'))

      currM = (psMissileCount s'')
      newM = length (getMissilesMinusTopRow (psBoard s''))
      updateScore = if newM >= currM then s'' else s'' { psScore = (Score.addVar (psScore s'') (Just X) (currM-newM)) }


      s''            = case psTypeCooldown s' of
        0 -> s'
        _ -> s' {psTypeCooldown = (psTypeCooldown s') - 1}
  Left _ -> halt (s) 


