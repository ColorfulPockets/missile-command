{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    Board
  , CellContents (..)
  , Pos (..)
  , Direct (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , remove
  , notNone
  , positions
  , emptyPositions
  , boardWinner
  
  , findCharPos

  , putAndRemove2
  , travel
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

import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos CellContents

checkMatch :: Board -> Char -> Pos -> Bool  -- check if the char is in this position
checkMatch board c p = case M.lookup p board of 
  Nothing -> False
  Just (O l) -> if l == c then True else False
  Just _ -> False

findCharPos :: Board -> Char -> [Pos] -- returns a list of positions for that letter
findCharPos board c = [ p | p <- positions, (checkMatch board c p)]

charArray :: [Char]
charArray = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

data CellContents 
  = X 
  | O { letter :: Char }
  | F {distance :: Int, dir :: Direct}
  | None
  deriving (Eq, Show)

data Direct
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe CellContents 
board ! pos = M.lookup pos board

dim :: Int
dim = 50

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

rTop :: [Pos]
rTop = [Pos r 4 | r <- [1..dim]] --TODO

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- rTop, M.notMember p board] -- TODO

notNone :: CellContents -> Bool
notNone c = case c of
      None  -> False
      _     -> True

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

-- List of columns that don't have a missile
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
  | Win CellContents
  | Retry 
  | Cont a
  | UpdateScore a
  deriving (Eq, Functor, Show)

put :: Board -> CellContents -> Pos -> Board
put board xo pos = case M.lookup pos board of 
  Just _  -> board
  Nothing -> M.insert pos xo board

--putAndRemove :: Board -> CellContents -> (Pos, Pos) -> Result Board
--putAndRemove board xo (pos, toRemove) = case M.lookup pos board of 
--  Just _  -> Retry
--  Nothing -> result (M.insert pos xo (fst (remove board toRemove)))

putAndRemove2 :: Board -> ([(Pos, CellContents)], [(Pos, CellContents)]) -> Board
putAndRemove2 board (pos, toRemove) = (iterI b' pos)
  where
    b' = iterR board toRemove


iterR :: Board -> [(Pos, CellContents)] -> Board
iterR b []       = b
iterR b ((pos, contents):xs) = iterR b' xs
  where
    b' = case contents of
      (O _) -> fst (remove b pos)
      _      -> b


iterI :: Board -> [(Pos, CellContents)] -> Board
iterI b []       = b
iterI b ((pos, contents):xs) = iterI b' xs
  where
    b' = case contents of 
      (O _) -> M.insert pos contents b
      _ -> b


remove :: Board -> Pos -> (Board, CellContents)
remove board pos = case M.lookup pos board of 
  Nothing -> (board, None)
  Just c  -> ((M.delete pos board), c)

result :: Board -> Result Board
result b 
  | isFull b  = Draw
  | wins b X  = Win  X 
--  | wins b O  = Win  O
  | otherwise = Cont b

wins :: Board -> CellContents -> Bool
wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

winsPoss :: Board -> CellContents -> [Pos] -> Bool
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

boardWinner :: Result a -> Maybe CellContents
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing




--------------------------------
-- STUFF PREVIOUSLY IN PLAYER -- 
--------------------------------

travel :: Board -> IO ([(Pos, CellContents)], [(Pos, CellContents)])
travel b = do
  posL <- trailHelper thingsWithCells b
  trail_deleted <- (delTrailIter posL)
  trailConvert trail_deleted
  where
    thingsOnBoard = thingPos b
    thingsWithCells = posWithCellContents thingsOnBoard b
    --p = trailHelper t b

posWithCellContents :: [Pos] -> Board -> [(Pos, CellContents)]
posWithCellContents [] _ = []
posWithCellContents (p:ps) b = case (b ! p) of
  Just c  -> (p, c) : (posWithCellContents ps b)
  _       -> posWithCellContents ps b

trailConvert :: [(a, a)] -> IO ([a], [a])
trailConvert p = return (unzip p)

-- The list of possible amounts for a missile to move left or right, biased toward straight down so that paths aren't as chaotic.
leftRightTravelAmounts :: [Int]
leftRightTravelAmounts = [-2,-1, -1, 0,0,0, 1,1,2]

genIndexOnBoard :: Int -> IO (Int)
genIndexOnBoard j = do
  index <- randomRIO (0, (length leftRightTravelAmounts) - 1)
  if (j + (leftRightTravelAmounts !! index)) < dim && (j + (leftRightTravelAmounts !! index)) >0
    then return index
  else genIndexOnBoard j


-- Takes a list of positions to delete, returns a list of pairs (toAdd, toRemove)
delTrailIter :: [(Pos, CellContents)] -> IO ([((Pos, CellContents), (Pos, CellContents))])
delTrailIter []               = return([])
delTrailIter (e@((Pos i j), c):xs) = do
  index <- (genIndexOnBoard j)
  newXs <- delTrailIter xs
  return ((((Pos (i + 1) (j + (leftRightTravelAmounts !! index))), c), e) : newXs)


-- @Bhavani: Generate new missile here by replacing the O
trailHelper :: [(Pos, CellContents)] -> Board -> IO [(Pos, CellContents)]
trailHelper [] b = do
                      i <- randomRIO (0,25)
                      return [((Pos 1 y), (O (charArray !! i)))]
  where
    (Pos _ y) = botThing b !! 0

trailHelper xs b = do
  i <- randomRIO (0, 50) :: IO Int
  if i == 0 then
    do
      (Pos _ y) <- fetcher b
      i <- randomRIO (0,25)
      return (if y == 0 then xs else ((Pos 1 y), (O (charArray !! i))) : xs)
  else
    return xs
--  where
--    (Pos _ y) = botThing b !! 0


-- Fetch a random column that doesn't have a missile
fetcher :: Board -> IO Pos
fetcher b = do
  allPos <- converter b
  case allPos of
    [] -> return (Pos 0 0)
    _  -> do
      i <- randomRIO (0, (length allPos - 1))
      return (allPos !! i)

converter :: Board -> IO [Pos]
converter b = return (botThing b)

--delTrail :: Board -> Pos -> IO (Pos, Pos)
--delTrail b (Pos i j) = return ((Pos (i + 1) j), (Pos i j))


fetchZero :: [a] -> IO a
fetchZero xs = do
  return (xs !! 0)

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

