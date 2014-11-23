module Demo.Links ( parseInput ) where

import Text.Read (readMaybe)
import Demo.Types
import qualified Data.Map as M (lookup)

{- I think there is a lot of refactoring-opportunity in here,
   especially concerning the use of monads, but we'll leave that
   for later.
   -}

{- There are two implemented functions for Step:
   1. arrow: "we're looking for an arrow next"
   2. box: "we're looking for a box next"
   -}
type Step = (MemSt, [Int]) -> Maybe Cell -> [DElem]

parseInput :: InputState -> Either String [DElem]
parseInput (s, ls) = fmap (parse (ls,[]) box) (testHead s)

-- I thought there'd be a convieniece function for Maybe -> Either...
testHead :: String -> Either String Int
testHead s = case readInt s of
               Just i -> Right i
               _ -> Left "The Head-Index is Invalid!"

parse :: ( MemSt -- The contents of the onscreen memory cells
         , [Int] ) -- the list of seen indexes (to check for loopbacks)
      -> Step -- The current "machine state" i.e. what its looking for
      -> Int -- The index we're looking at right now
      -> [DElem] -- the list of diagram elements we're after
parse st step i = let m = fst st
                      cell = M.lookup i m
                  in step (saw i st) cell

box :: Step
box st (Just (i,val)) = (Box, (show i), val) : parse st arrow (i+1)
box _ _ = []

{- Failure for arrow: (end list)
   1. the arrow cell is empty/non-existant
   2. the value of the arrow cell is not an Int
   2. the cell which the arrow points to is empty/invalid
   Special failure: (add loopback and end list)
   3. the cell which the arrow points to is already seen
   -}
arrow :: Step
arrow (m,s) (Just (i,val)) = 
  -- first test for a valid next-index (check that its an int)
  case readInt val of
              -- then check whether it has been seen before
    Just n -> case getIndex n s of
                Just x -> [(LoopBack x, show i, val)]
                     -- finally check if it points to a valid box
                _ -> tryIns (Arrow, show i, val)
                            (parse (m,s) box n)
    _ -> []
-- of course, if the cell didnt exist in the first place, return []
arrow _ _ = []

tryIns :: a -> [a] -> [a]
tryIns a [] = []
tryIns a xs = a:xs

readInt :: String -> Maybe Int
readInt = readMaybe

saw :: Int -> (MemSt, [Int]) -> (MemSt, [Int])
saw i (m,s) = (m, i:s)

getIndex :: (Eq a) => a -> [a] -> Maybe Int
getIndex = r 0
  where r i a (x:xs) = if a == x
                          then Just i
                          else r (i+1) a xs
        r _ _ _ = Nothing
