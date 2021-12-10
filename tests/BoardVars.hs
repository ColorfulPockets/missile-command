module BoardVars where

import Model.Board

import Data.Maybe
import qualified Data.Map as M


-- One missile movement test, also used in posWithCellContents, getMissiles tests

simpleBoard :: Board
simpleBoard = M.fromList sbOld

sbShift :: ([(Pos, CellContents)], [(Pos, CellContents)])
sbShift = (sbNew, sbOld)

sbResult :: Board
sbResult = M.fromList sbNew

sbNew :: [(Pos, CellContents)]
sbNew = 
    [
        (Pos 4 8, O 'x')
    ]

sbOld :: [(Pos, CellContents)]
sbOld = 
    [
        (Pos 3 8, O 'x')
    ]

sbPos :: [Pos] -- posWithCellContents, getMissiles
sbPos = 
    [
        Pos 3 8
    ]

-- Multiple movement test, also used in posWithCellContents, getMissiles tests

advancedBoard :: Board
advancedBoard = M.fromList advOld

advShift :: ([(Pos, CellContents)], [(Pos, CellContents)])
advShift = (advNew, advOld)

advResult :: Board
advResult = M.fromList advNew

advNew :: [(Pos, CellContents)]
advNew = 
    [
        (Pos 7 49, O 'W'),
        (Pos 4 42, O 'E'),
        (Pos 30 4, O 'F'),
        (Pos 2 1, O 'Q')
    ]

advOld :: [(Pos, CellContents)]
advOld = 
    [
        (Pos 1 1, O 'Q'),
        (Pos 3 42, O 'E'),
        (Pos 6 49, O 'W'),
        (Pos 29 4, O 'F')
    ]

advPos :: [Pos] -- posWithCellContents, getMissiles
advPos = 
    [
        Pos 1 1,
        Pos 3 42,
        Pos 6 49,
        Pos 29 4
    ]

-- isMissile Check

msX :: Maybe CellContents
msX = listToMaybe [X]

msO :: Maybe CellContents
msO = listToMaybe [O 'Z']

msF :: Maybe CellContents
msF = listToMaybe [F 1 2 DirUp]

msN :: Maybe CellContents
msN = listToMaybe [None]

-- posWithCellContents, getMissiles - lots of things

pwccBoard :: Board
pwccBoard = M.fromList pwccContents

pwccPos :: [Pos]
pwccPos = 
    [
        Pos 1 2,
        Pos 3 4,
        Pos 19 39,
        Pos 41 2,
        Pos 37 24
    ]

pwccMissiles :: [Pos] -- getMissiles
pwccMissiles = 
    [
        Pos 1 2,
        Pos 41 2
    ]

pwccContents :: [(Pos, CellContents)]
pwccContents = 
    [
        (Pos 1 2, O 'x'),
        (Pos 3 4, F 1 2 DirDown),
        (Pos 19 39, X),
        (Pos 41 2, O 'Z'),
        (Pos 37 24, None)
    ]