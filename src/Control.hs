module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import qualified Data.Map as M 

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
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

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


shootSurrounding :: Board -> Pos -> Board
shootSurrounding b p = explodeAround p b'''''2
  where
    (b',      _)       = remove b p
    (b''    , c'')      = remove b'    (up p) -- up
    b''2               = case c'' of 
      (O _)   ->  shootSurrounding b'' (up p) 
      _       -> b''
    (b'''   , c''')     = remove b''2   (down p) -- down
    b'''2              = case c''' of
      (O _)   -> shootSurrounding b''' (down p)
      _       ->  b'''
    (b''''  , c'''')    = remove b'''2  (left p) -- left
    b''''2             = case c'''' of
      (O _)   ->  shootSurrounding b'''' (left p) 
      _       ->  b''''
    (b''''' , c''''')   = remove b''''2 (right p) -- right
    b'''''2            = case c''''' of
      (O _)   -> shootSurrounding b''''' (right p) 
      _       -> b'''''

-- Generates the initial explosion ring around a shot missile
explodeAround :: Pos -> Board -> Board
explodeAround p b = b'''''
  where 
    b' = put b (F 1 DirUp) (up p) -- up
    b'' = put b'  (F 1 DirDown) (down p) -- down
    b''' = put b'' (F 1 DirLeft) (left p) -- left
    b'''' = put b''' (F 1 DirRight) (right p) -- right
    b''''' = put b'''' (F 3 DirUp) p

-- Finds all explosions on the board and propogates them outward
moveExplosions :: Board -> Board
moveExplosions b = moveEachExplosion fs b
  where
    fs = getFs (M.toList b)

-- Helper function -- takes all explosion positions and propogates them outward if necessary
moveEachExplosion :: [Pos] -> Board -> Board
moveEachExplosion fs b = case fs of
  []  -> b
  (p : ps) -> case b ! p of
    Just (F i d) -> if i < 3 
      then moveEachExplosion ps (propogateInDirection p i d b' )
      else moveEachExplosion ps b'
      where 
        (b', _) = remove b p
    _       -> moveEachExplosion ps b

{-
Propogates a single explosion piece, increasing it's distance counter.

Up: Propogates up and right
Right: Propogates right and down
Down: Propogates down and left
Left: Propogates left and up

Essentially, each type of F has its own quadrant so it won't collide with itself.
The distance counter is used for explosion radius, see moveEachExplosion which checks
if it's above a certain amount
-}
propogateInDirection :: Pos -> Int -> Direct -> Board -> Board
propogateInDirection p i d b = 
  case d of
    DirUp         ->  propogateQuadrant b p DirUp i up right
    DirRight      ->  propogateQuadrant b p DirRight i right down
    DirDown       ->  propogateQuadrant b p DirDown i down left
    DirLeft       ->  propogateQuadrant b p DirLeft i left up
    
propogateQuadrant :: Board -> Pos -> Direct -> Int -> (Pos -> Pos) -> (Pos -> Pos) -> Board
propogateQuadrant b p dir i posDir1 posDir2 = b''''
  where 
    (b', c')     = remove b (posDir1 p)
    b'2          = case c' of 
      (O _) -> shootSurrounding b' (posDir1 p) 
      _     -> b'
    (b'', c'')    = remove b'2 (posDir2 p)
    b''2          = case c'' of 
      (O _) -> shootSurrounding b'' (posDir2 p) 
      _     -> b''
    b'''        = put b''2 (F (i+1) dir) (posDir1 p)
    b''''       = put b''' (F (i+1) dir) (posDir2 p)

-- Returns a list of Pos where the CellContents is F
getFs :: [(Pos, CellContents)] -> [Pos]
getFs b = case b of
  []  -> []
  ((p, c) : t) -> case c of
    (F _ _) -> p : (getFs t)
    _       -> getFs t

getPos :: PlayState -> IO ([(Pos, CellContents)], [(Pos, CellContents)])
getPos s = do
  (p, del) <- travel (psBoard s)
  return (p, del)

-- This function controls how things on the board change.
-- If you want to change the board, start here.
-------------------------------------------------------------------------------
progressBoard :: PlayState -> IO (Board)
-------------------------------------------------------------------------------
progressBoard s = do
    b <- putAndRemove2 (psBoard s) <$> getPos s -- this line moves all the misiles downward
    return (moveExplosions b)
  -- Add other lines here for anything in the board state that should change every tick (such as explosion animations)


-- TODO: This is where the score should be updated, I think
-------------------------------------------------------------------------------
nextS :: PlayState -> Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s (result b) of
  Right s' -> continue s'
  Left res -> halt (s) 


