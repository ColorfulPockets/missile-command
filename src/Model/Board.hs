{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    Board
  , XO (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , remove
  , positions
  , emptyPositions
  , boardWinner
  , flipXO
  
  , putAndRemove2
  , result

  , thingPos
  , botThing

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos XO

data XO 
  = X 
  | O
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe XO 
board ! pos = M.lookup pos board

dim :: Int
dim = 50

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

rTop :: [Pos]
rTop = [Pos r 4 | r <- [1..dim]] --TODO

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- rTop, M.notMember p board] -- TODO

--emptyPositions :: Board -> [Pos]
--emptyPositions board  = [ p | p <- positions, M.notMember p board]

mostPos :: [Pos]
mostPos = [Pos r c | r <- [1..(dim - 1)], c <- [1..dim]]

thingPos :: Board -> [Pos]
thingPos board = [p | p <- mostPos, M.member p board]

botRow :: [Pos]
botRow = [Pos dim c | c <- [1..dim]]

--botThing :: Board -> [Pos]
--botThing board = [p | p <- botRow, M.notMember p board]

botThing :: Board -> [Pos]
botThing b = [Pos dim c | c <- [1..dim], notIn b dim c]

notIn :: Board -> Int -> Int -> Bool
notIn b 0 _ = True
notIn b r c = if M.notMember (Pos r c) b then notIn b (r - 1) c else False

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Draw 
  | Win XO
  | Retry 
  | Cont a
  | UpdateScore a
  deriving (Eq, Functor, Show)

put :: Board -> XO -> Pos -> Result Board
put board xo pos = case M.lookup pos board of 
  Just _  -> Retry
  Nothing -> result (M.insert pos xo board)

--putAndRemove :: Board -> XO -> (Pos, Pos) -> Result Board
--putAndRemove board xo (pos, toRemove) = case M.lookup pos board of 
--  Just _  -> Retry
--  Nothing -> result (M.insert pos xo (fst (remove board toRemove)))

putAndRemove2 :: Board -> XO -> ([Pos], [Pos]) -> Result Board
putAndRemove2 board xo (pos, toRemove) = result (iterI b' xo pos)
  where
    b' = iterR board toRemove


iterR :: Board -> [Pos] -> Board
iterR b []       = b
iterR b (pos:xs) = iterR b' xs
  where
    b' = fst (remove b pos)


iterI :: Board -> XO -> [Pos] -> Board
iterI b xo []       = b
iterI b xo (pos:xs) = iterI b' xo xs
  where
    b' = M.insert pos xo b


remove :: Board -> Pos -> (Board, Bool)
remove board pos = case M.lookup pos board of 
  Nothing -> (board, False)
  Just _  -> ((M.delete pos board), True)

result :: Board -> Result Board
result b 
  | isFull b  = Draw
  | wins b X  = Win  X 
  | wins b O  = Win  O
  | otherwise = Cont b

wins :: Board -> XO -> Bool
wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

winsPoss :: Board -> XO -> [Pos] -> Bool
winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

winPositions :: [[Pos]]
winPositions = rows ++ cols ++ diags 

rows, cols, diags :: [[Pos]]
rows  = [[Pos r c | c <- [1..dim]] | r <- [1..dim]]
cols  = [[Pos r c | r <- [1..dim]] | c <- [1..dim]]
diags = [[Pos i i | i <- [1..dim]], [Pos i (dim+1-i) | i <- [1..dim]]]

isFull :: Board -> Bool
isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = min dim (pRow p + 1) 
  } 

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min dim (pCol p + 1) 
  } 

boardWinner :: Result a -> Maybe XO
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing

flipXO :: XO -> XO
flipXO X = O
flipXO O = O --TODO

