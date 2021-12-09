{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)
import Model.Board
import qualified Data.Map as M 

main :: IO ()
main = runTests 
  [ boardTests
  -- , probFold
  ]

-- probFold ::  Score -> TestTree
-- probFold sc = testGroup "Problem 1: Fold" 
--   [ scoreTest ((\_ -> myReverse [1,2,3,4,5]),     (), [5,4,3,2,1], 5,   "rev-1")
--   , scoreTest ((\_ -> myFoldr (-) 0 [1,2,3,4,5]), (),           3, 5, "foldr-1")
--   , scoreTest ((\_ -> myFoldl (-) 0 [1,2,3,4,5]), (),       (-15), 5, "foldl-1")
--   ]
--   where
--     scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
--     scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

boardTests ::  Score -> TestTree
boardTests sc = testGroup "Board module"
  [ scoreTest (down, (Pos 1 1),  (Pos 2 1)                      , 2, "down")
  -- , scoreTest ((\_ -> eval store0 (Val  (IntVal 92))), (),  IntVal 92               , 2, "eval-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

-- probParse ::  Score -> TestTree
-- probParse sc = testGroup "Problem 3: Parse"
--   [ scoreTestI ((\_ -> parseFile "test/in/fact.imp"), (), Right w_fact, 5, "parse-1")
--   , scoreTestI ((\_ -> parseFile "test/in/abs.imp"), (), Right w_abs, 5, "parse-1")
--   , scoreTestI ((\_ -> parseFile "test/in/times.imp"), (), Right w_times, 5, "parse-1")
--   , scoreTestI ((\_ -> parseFile "test/in/test.imp"), (), Right w_test, 5, "parse-1")
--   ]
--   where
--     scoreTestI :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
--     scoreTestI (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
