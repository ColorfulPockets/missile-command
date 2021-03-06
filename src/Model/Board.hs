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
  , explosionRadius
  , explodeAround
  , initialTimer
  , (!)
  , init
  , put
  , remove
  , notNone
  , notIn
  , positions
  , getFs
  
  , findCharPos
  , getMissiles
  , getMissilesMinusTopRow
  , bottomRowHasMissile

  , putAndRemove
  , travel
  , result

    -- * Moves
  , up
  , down
  , left
  , right

  -- Control
  , shootSurrounding
  , moveExplosions

  -- Visual
  , gameOverBoard

  -- For testing
  , isMissile
  , posWithCellContents
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
  Just (Ms l) -> l == c
  _          -> False

findCharPos :: Board -> Char -> [Pos] -- returns a list of positions for that letter
findCharPos board c = [ p | p <- positions, checkMatch board c p]

data CellContents 
  = X 
  | Ms { letter :: Char }
  | Fire {distance :: Int, timer :: Int, dir :: Direct}
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
  deriving (Eq, Ord, Show)

(!) :: Board -> Pos -> Maybe CellContents 
board ! pos = M.lookup pos board

dim :: Int
dim = 49

explosionRadius :: Int
explosionRadius = 10

initialTimer :: Int
initialTimer = 10

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

-- tested
notNone :: CellContents -> Bool
notNone c = case c of
      None  -> False
      _     -> True

playAreaPos :: [Pos]
playAreaPos = [Pos r c | r <- [1..(dim - 1)], c <- [1..dim]]

itemsPos :: Board -> [Pos]
itemsPos board = [p | p <- playAreaPos, M.member p board]

-- List of columns that don't have a missile
uniqueCols :: Board -> IO [Pos]
uniqueCols b = return [Pos dim c | c <- [1..dim], notIn b dim c]

-- tested
notIn :: Board -> Int -> Int -> Bool
notIn _ 0 _ = True
notIn b r c = if M.notMember (Pos r c) b then notIn b (r - 1) c 
  else case b ! (Pos r c) of
    Just (Ms _) -> False
    _ -> True

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Win CellContents
  | Lose
  | Retry 
  | Cont a
  | UpdateScore a
  deriving (Eq, Functor, Show)

put :: Board -> CellContents -> Pos -> Board
put board xo pos@(Pos r c) = case M.lookup pos board of 
  Just _  -> board
  Nothing 
    | r <= dim && r > 0 && c <= dim && c > 0 -> M.insert pos xo board
    | otherwise -> board

-- tested -- Eric
putAndRemove :: Board -> ([(Pos, CellContents)], [(Pos, CellContents)]) -> Board
putAndRemove board (pos, toRemove) = iterI b' pos
  where
    b' = iterR board toRemove


iterR :: Board -> [(Pos, CellContents)] -> Board
iterR b []       = b
iterR b ((pos, contents):xs) = iterR b' xs
  where
    b' = case contents of
      (Ms _) -> fst (remove b pos)
      _     -> b


iterI :: Board -> [(Pos, CellContents)] -> Board
iterI b []       = b
iterI b ((pos, contents):xs) = case b ! pos of
  Just (Fire _ _ _) -> iterI b' xs
    where
      b' =  case contents of
        (Ms _) -> shootSurrounding b pos
        _     -> b

  _ -> iterI b' xs
    where
      b' = case contents of 
        (Ms _) -> M.insert pos contents b
        _     -> b

-- tested
remove :: Board -> Pos -> (Board, CellContents)
remove board pos = case M.lookup pos board of 
  Nothing -> (board, None)
  Just c  -> ((M.delete pos board), c)

-- tested
result :: Board -> Result Board
result b 
  | bottomRowHasMissile b = Lose
  | gameOverDisplayed b   = Lose
  | otherwise = Cont b

bottomRow :: [Pos]
bottomRow = [Pos dim c | c <- [1..dim]]

-- tested -- Eric
isMissile :: Maybe CellContents -> Bool
isMissile c = case c of
  Just (Ms _) -> True
  _   -> False

isX :: Maybe CellContents -> Bool
isX c = case c of
  Just X -> True
  _   -> False

-- tested
bottomRowHasMissile :: Board -> Bool
bottomRowHasMissile b = elem True (fmap isMissile (fmap (b !) bottomRow))

gameOverDisplayed :: Board -> Bool
gameOverDisplayed b = elem True (fmap isX (fmap (b !) playAreaPos))

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------
-- tested
up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

-- tested
down :: Pos -> Pos
down p = p 
  { pRow = min dim (pRow p + 1) 
  } 

-- tested
left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

-- tested
right :: Pos -> Pos 
right p = p 
  { pCol = min dim (pCol p + 1) 
  } 

--------------------------------
-- STUFF PREVIOUSLY IN PLAYER -- 
--------------------------------

travel :: Board -> Int -> IO ([(Pos, CellContents)], [(Pos, CellContents)])
travel b n = do
  posL <- trailHelper thingsWithCells b n
  trail_deleted <- (delTrailIter posL)
  trailConvert trail_deleted
  where
    thingsOnBoard = itemsPos b
    thingsWithCells = posWithCellContents thingsOnBoard b

-- tested -- Eric
-- Takes a list of Pos, and a board, and returns a list of (Pos, CellContents) at those pos on the board
posWithCellContents :: [Pos] -> Board -> [(Pos, CellContents)]
posWithCellContents [] _ = []
posWithCellContents (p:ps) b = case b ! p of
  Just c -> (p, c) : posWithCellContents ps b
  _      -> posWithCellContents ps b

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


-- Spawns new missiles
trailHelper :: [(Pos, CellContents)] -> Board -> Int -> IO [(Pos, CellContents)]
trailHelper [] b _ = do
                      c <- randomRIO ('A', 'Z') :: IO Char
                      (Pos _ y) <- fetcher b
                      return [((Pos 1 y), (Ms c))]

trailHelper xs b n = do
  i <- randomRIO (0, n) :: IO Int
  if i == 0 then
    do
      (Pos _ y) <- fetcher b
      c <- randomRIO ('A', 'Z') :: IO Char
      return (if y == 0 then xs else ((Pos 1 y), (Ms c)) : xs)
  else
    return xs


-- Fetch a random column that doesn't have a missile
fetcher :: Board -> IO Pos
fetcher b = do
  allPos <- uniqueCols b
  case allPos of
    [] -> return (Pos 0 0)
    _  -> do
      i <- randomRIO (0, (length allPos - 1))
      return (allPos !! i)


------------------
-- From Control --
------------------

shootSurrounding :: Board -> Pos -> Board
shootSurrounding b p = explodeAround p b'''''2
  where
    (b',      _)       = remove b p
    (b''    , c'')     = remove b'    (up p) -- up

    b''2               = case c'' of 
      (Ms _)   ->  shootSurrounding b'' (up p) 
      _       -> b''
    (b'''   , c''')    = remove b''2   (down p) -- down

    b'''2              = case c''' of
      (Ms _)   -> shootSurrounding b''' (down p)
      _       ->  b'''
    (b''''  , c'''')   = remove b'''2  (left p) -- left

    b''''2             = case c'''' of
      (Ms _)   ->  shootSurrounding b'''' (left p) 
      _       ->  b''''
    (b''''' , c''''')  = remove b''''2 (right p) -- right

    b'''''2            = case c''''' of
      (Ms _)   -> shootSurrounding b''''' (right p) 
      _       -> b'''''

-- Generates the initial explosion ring around a shot missile
-- tested
explodeAround :: Pos -> Board -> Board
explodeAround p b = b'''''
  where 
    b' = put b (Fire 1 initialTimer DirUp) (up p) -- up
    b'' = put b'  (Fire 1 initialTimer DirDown) (down p) -- down
    b''' = put b'' (Fire 1 initialTimer DirLeft) (left p) -- left
    b'''' = put b''' (Fire 1 initialTimer DirRight) (right p) -- right
    b''''' = put b'''' (Fire explosionRadius initialTimer DirUp) p
-- Remember -- CellContents F have format:
--    (Fire radius timer direction)
--    Timer counts down from the radius so that the farthest cells are filled for the shortest time
--    Radius counts up to the radius to know when it has reached it

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
    Just (Fire i t d) -> 
      if i < explosionRadius 
      then moveEachExplosion ps (propogateInDirection p i t d b' )
      else moveEachExplosion ps b'
      where 
        b' = case t of
          0   -> b''
          _   -> put b'' (Fire explosionRadius (t-1) d) p 
          -- setting the radius to max means this won't propogate after the first time it does
        (b'', _) = remove b p
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
propogateInDirection :: Pos -> Int -> Int -> Direct -> Board -> Board
propogateInDirection p i t d b = 
  case d of
    DirUp         ->  propogateQuadrant b p DirUp i t up right
    DirRight      ->  propogateQuadrant b p DirRight i t right down
    DirDown       ->  propogateQuadrant b p DirDown i t down left
    DirLeft       ->  propogateQuadrant b p DirLeft i t left up


{-
      U
    L U U
  L L C R R
    D D R
      D
-} 
    
propogateQuadrant :: Board -> Pos -> Direct -> Int -> Int -> (Pos -> Pos) -> (Pos -> Pos) -> Board
propogateQuadrant b p dir i t posDir1 posDir2 = b''''
  where 
    (b', c')     = remove b (posDir1 p)
    b'2          = case c' of 
      (Ms _) -> shootSurrounding b' (posDir1 p) 
      _     -> b'
    (b'', c'')    = remove b'2 (posDir2 p)
    b''2          = case c'' of 
      (Ms _) -> shootSurrounding b'' (posDir2 p) 
      _     -> b''
    b'''        = put b''2 (Fire (i+1) (t-1) dir) (posDir1 p)
    b''''       = put b''' (Fire (i+1) (t-1) dir) (posDir2 p)

-- Returns a list of Pos where the CellContents is F
-- tested
getFs :: [(Pos, CellContents)] -> [Pos]
getFs b = case b of
  []  -> []
  ((p, c) : t) -> case c of
    (Fire _ _ _) -> p : (getFs t)
    _       -> getFs t


--------------------------------

-- generates pixel art based on the string, offset by int inputs.
mkLetter :: String -> Int -> Int -> [Pos]
mkLetter s@(char : chars) r c = if char == '*' 
  then (Pos r c) : (mkLetter chars newr newc)
  else  mkLetter chars newr newc
    where
      newc = 
        if ((length s) == 1) 
          || ((length s) == 6)  
          || ((length s) == 11) 
          || ((length s) == 16) 
          || ((length s) == 21)
          || ((length s) == 26)
          || ((length s) == 31)
        then c - 4
        else c + 1
      newr = 
        if ((length s) == 1) 
          || ((length s) == 6)  
          || ((length s) == 11) 
          || ((length s) == 16) 
          || ((length s) == 21)
          || ((length s) == 26)
          || ((length s) == 31)
        then r + 1
        else r
mkLetter _ _ _  = []

-- generates pixel art based on the string, offset by int inputs.
mkNumber :: String -> Int -> Int -> [Pos]
mkNumber s@(char : chars) r c = if char == '*' 
  then (Pos r c) : (mkNumber chars newr newc)
  else  mkNumber chars newr newc
    where
      newc = 
        if ((length s) == 1) 
          || ((length s) == 4)  
          || ((length s) == 7) 
          || ((length s) == 10) 
          || ((length s) == 13)
          || ((length s) == 16)
          || ((length s) == 19)
        then c - 2
        else c + 1
      newr = 
        if ((length s) == 1) 
          || ((length s) == 4)  
          || ((length s) == 7) 
          || ((length s) == 10) 
          || ((length s) == 13)
          || ((length s) == 16)
          || ((length s) == 19)
        then r + 1
        else r
mkNumber _ _ _  = []

endScreen :: [Pos]
endScreen = 
  (mkLetter g i j)
  ++ (mkLetter a (i) (1*offsetH + j))
  ++ (mkLetter m (i) (2*offsetH + j))
  ++ (mkLetter e (i) (3*offsetH + j))
  ++ (mkLetter o (offsetV + i) (j))
  ++ (mkLetter v (offsetV + i) (1*offsetH + j))
  ++ (mkLetter e (offsetV + i) (2*offsetH + j))
  ++ (mkLetter r (offsetV + i) (3*offsetH + j))
  ++ (mkLetter s (3*offsetV + i - 4) (j - 3))
  ++ (mkLetter c (3*offsetV + i - 4) (1*offsetH + j - 3))
  ++ (mkLetter o (3*offsetV + i - 4) (2*offsetH + j - 3))
  ++ (mkLetter r (3*offsetV + i - 4) (3*offsetH + j - 3))
  ++ (mkLetter e (3*offsetV + i - 4) (4*offsetH + j - 3))
    where
      g = " *****    *    *  ***   **   * *** "
      a = "  *   * * *   **   *******   **   *"
      m = "*   *** *** * **   **   **   **   *"
      e = "******    *    ******    *    *****"
      o = " *** *   **   **   **   **   * *** "
      v = "*   **   **   **   **   * * *   *  "
      r = "**** *   **   ***** *   **   **   *"
      s = " *****    *     ***     *    ***** "
      c = " *****    *    *    *    *     ****"
      i = 6
      j = 14
      offsetH = 6
      offsetV = 9

gameOverBoard :: Int -> Board
gameOverBoard score = addScoreTo (gameOverHelper M.empty endScreen) score

gameOverHelper :: Board -> [Pos] -> Board
gameOverHelper b [] = b
gameOverHelper b (p:ps) = gameOverHelper (put b X p) ps

addScoreTo :: Board -> Int -> Board
addScoreTo b sc = gameOverHelper b (scoreLetters score)
  where 
    score = show sc

scoreLetters :: String -> [Pos]
scoreLetters s =  mkNumbers s 0 (length s)

mkNumbers :: String -> Int -> Int -> [Pos]
mkNumbers (char : chars) position count = 
  let
    str1 = " * **  *  *  *  * ***"
    str2 = "***  *  *****  *  ***"
    str3 = "***  *  * **  *  ****"
    str4 = "* ** ** ****  *  *  *"
    str5 = "****  *  ***  *  ****"
    str6 = "****  *  **** ** ****"
    str7 = "***  *  *  *  *  *  *"
    str8 = "**** ** ***** ** ****"
    str9 = "**** ** ****  *  *  *"
    str0 = "**** ** ** ** ** ****"
    row = 38
    col = 25 - 2*(count)
    offset = 4
  in
    case char of 
    '1' -> (mkNumber str1 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '2' -> (mkNumber str2 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '3' -> (mkNumber str3 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '4' -> (mkNumber str4 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '5' -> (mkNumber str5 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '6' -> (mkNumber str6 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '7' -> (mkNumber str7 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '8' -> (mkNumber str8 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '9' -> (mkNumber str9 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    '0' -> (mkNumber str0 row (col + position*offset)) ++ (mkNumbers chars (position + 1) count)
    _   -> mkNumbers chars (position + 1) count

mkNumbers [] _ _ = []

isMissileBoard :: Board -> Pos -> Bool  -- check if the content is a missile
isMissileBoard board p = isMissile (M.lookup p board)

-- tested -- Eric
getMissiles :: Board -> [Pos] -- returns a list of positions with missiles
getMissiles board = [ p | p <- positions, isMissileBoard board p]

-- tested
getMissilesMinusTopRow :: Board -> [Pos] -- returns a list of positions with missiles minus the top row
getMissilesMinusTopRow board = [ p | p <- boardMinusTopRow, isMissileBoard board p]

boardMinusTopRow ::  [Pos] -- returns a list of positions minus the top row
boardMinusTopRow  = [ Pos r c | r <- [2..dim], c <- [1..dim] ] 
