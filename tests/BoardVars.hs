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
        (Pos 4 8, Ms 'x')
    ]

sbOld :: [(Pos, CellContents)]
sbOld = 
    [
        (Pos 3 8, Ms 'x')
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
        (Pos 7 49, Ms 'W'),
        (Pos 4 42, Ms 'E'),
        (Pos 30 4, Ms 'F'),
        (Pos 2 1, Ms 'Q')
    ]

advOld :: [(Pos, CellContents)]
advOld = 
    [
        (Pos 1 1, Ms 'Q'),
        (Pos 3 42, Ms 'E'),
        (Pos 6 49, Ms 'W'),
        (Pos 29 4, Ms 'F')
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
msO = listToMaybe [Ms 'Z']

msF :: Maybe CellContents
msF = listToMaybe [Fire 1 2 DirUp]

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
        (Pos 1 2, Ms 'x'),
        (Pos 3 4, Fire 1 2 DirDown),
        (Pos 19 39, X),
        (Pos 41 2, Ms 'Z'),
        (Pos 37 24, None)
    ]