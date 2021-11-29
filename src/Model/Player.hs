module Model.Player where

import Model.Board
import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { plName  :: String 
  , plStrat :: Strategy
  } 

type Strategy = Pos     -- ^ current cursor
             -> Board   -- ^ current board
             -> XO      -- ^ naught or cross
             -> IO (Pos, Maybe Pos)  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return (p, Nothing))

rando :: Player 
rando = Player "machine" travel

--randomStrategy :: a -> Board -> b -> IO Pos
--randomStrategy _ b _ = fetchZero (emptyPositions b)

travel :: a -> Board -> b -> IO (Pos, Maybe Pos)
travel _ b _ = delTrail b p
  where
    t = thingPos b
    p = trailHelper t b


trailHelper :: [Pos] -> Board -> Pos
trailHelper [] b = Pos 1 y
  where
    (Pos _ y) = botThing b !! 0

trailHelper (x:xs) b = x


delTrail :: Board -> Pos -> IO (Pos, Maybe Pos)
delTrail b (Pos i j) = return ((Pos (i + 1) j), Just (Pos i j))


deleteTrail :: Board -> Int -> IO (Pos, Maybe Pos)
deleteTrail b i
  | i < dim = case b ! (Pos i 4) of

    Just _ -> return (Pos (i + 1) 4, Just (Pos i 4))

    _      -> deleteTrail b (i + 1)

  | otherwise = return (Pos 1 4, Nothing)


fetchZero :: [a] -> IO a
fetchZero xs = do
  return (xs !! 0)

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)