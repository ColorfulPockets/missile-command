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
             -> IO ([Pos], [Pos])  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return ([p], []))

rando :: Player 
rando = Player "machine" travel

--randomStrategy :: a -> Board -> b -> IO Pos
--randomStrategy _ b _ = fetchZero (emptyPositions b)

travel :: a -> Board -> b -> IO ([Pos], [Pos])
travel _ b _ = do
  posL <- trailHelper t b
  trail_deleted <- (delTrailIter posL)
  trailConvert trail_deleted
  where
    t = thingPos b
    --p = trailHelper t b


trailConvert :: [(Pos, Pos)] -> IO ([Pos], [Pos])
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


delTrailIter :: [Pos] -> IO ([(Pos, Pos)])
delTrailIter []               = return([])
delTrailIter (e@(Pos i j):xs) = do
  index <- (genIndexOnBoard j)
  newXs <- delTrailIter xs
  return (((Pos (i + 1) (j + (leftRightTravelAmounts !! index))), e) : newXs)


trailHelper :: [Pos] -> Board -> IO [Pos]
trailHelper [] b = return [Pos 1 y]
  where
    (Pos _ y) = botThing b !! 0

trailHelper xs b = do
  i <- randomRIO (0, 50) :: IO Int
  if i == 0 then
    do
      (Pos _ y) <- fetcher b
      return (if y == 0 then xs else (Pos 1 y) : xs)
  else
    return xs
--  where
--    (Pos _ y) = botThing b !! 0


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