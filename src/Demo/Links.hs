module Demo.Links ( parseInput
                  , randomInput
                  , getRect
                  , findScale 
                  , scaleMap
                  , nmap
                  , mkLayout ) where

import Text.Read (readMaybe)
import System.Random
import Demo.Types
import qualified Data.Map as M (empty, lookup)
import qualified Data.List as L (delete, length)

-- Config!
randomEmptyCells = 2 :: Int
randomValueRange = ('A','Z')

{- I think there is a lot of refactoring-opportunity in here,
   especially concerning the use of monads, but we'll leave that
   for later.
   -}

type CellPair = (Int, Int)

randomInput :: Int -> Int -> IO InputState
randomInput start size = 
  do let values = take size [start..]
         pairs (v:a:cs) = (v,a) : pairs cs
         pairs _ = [] 
     gen <- newStdGen
     let fvs = foldr L.delete values (getEmpty randomEmptyCells gen values)
         (headV,mem) = mkMem start size (shuffle (pairs  fvs) gen)
                             (randomRs randomValueRange gen)
     return ( InSt start size headV mem)

shuffle :: RandomGen g => [a] -> g -> [a]
shuffle as _ = as -- TODO

mkMem :: Int -> Int -> [CellPair] -> [Char] -> (String, MemSt)
mkMem i s cs vals = 
  let h = show (fst (head cs))
      r ((v,a) : (v2,a2) : xs) (k:ks) = (v,k) : (a,show v2) : r ((v2,a2):xs) ks
      r ((v,a) : []) (k:ks) = (v,k) : (a,show (i + s + 4)) : []
      --r _ _ = []
  in (h, mkMemSt (r cs (fmap (: []) vals)))

getEmpty n g xs = 
  if n > 0
     then let (a,ng) = pickR (emptyChoices xs) g
              zs = breakLs a xs
          in a : getEmpty (n - 1) ng zs
     else []

emptyChoices (x:y:xs) = x : emptyChoices xs
emptyChoices _ = []

breakLs a (x:xs) = if a == x
                      then xs
                      else breakLs a xs

pickR xs g = (head xs, g)
{-
pickR :: RandomGen g => [a] -> g -> (a, g)
pickR xs g = let r _ (x:[]) = x
                 r i (x:xs) = if i > 0
                                 then r (i - 1) xs
                                 else x
                 (i, g2) = randomR (0, (L.length xs) - 1) g
             in (r i xs, g2)
-}



{- There are two implemented functions for Step:
   1. arrow: "we're looking for an arrow next"
   2. box: "we're looking for a box next"
   -}
type Step = (MemSt, [Int]) -> Maybe Cell -> [DElem]

parseInput :: InputState -> Either String [DElem]
parseInput a = fmap (parse ((memVals a),[]) box) (testHead a)

-- I thought there'd be a convieniece function for Maybe -> Either...
testHead :: InputState -> Either String Int
testHead s = case readInt (headVal s) of
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
box st (Just (i,val)) = 
  let continue = (Box, (show i), val) : parse st arrow (i + 1)
  in if val == "" -- if empty, we must check to see if next-addr is empty
        then case M.lookup (i + 1) (fst st) of
               -- if it is empty, we don't draw the box
               Nothing -> []
               Just (_,"") -> []
               _ -> continue 
        else continue
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

mkLayout :: Diagram -> Layout
mkLayout = flow (0,0)

flow :: (Int, Int) -> [DElem] -> [LElem]
flow (x,y) (a:as) = bound a (x,y) : flow ((fst (sizeOf a)) + x, y) as
flow _ _ = []

bound :: DElem -> (Int, Int) -> LElem
bound elem (x,y) = let (xo, yo) = sizeOf elem
                   in (elem, (x,y), (xo,yo))

sizeOf :: DElem -> (Int, Int)
sizeOf (Box,_,_) = (2,3)
sizeOf (Arrow,_,_) = (2,3)
sizeOf (LoopBack _,_,_) = (2,5)

getRect :: Layout -> (Int, Int)
getRect = foldr f (0,0)
  where f :: LElem -> (Int, Int) -> (Int, Int)
        f (_, (x,y), (xo,yo)) (a,b) = (max (x+xo) a, max (y+yo) b)


findScale :: (Double, Double) -> (Int, Int) -> Double
findScale (cw,ch) (lw,lh) = let lwd = fromIntegral lw
                                lhd = fromIntegral lh
                                cwd = cw - 10
                                chd = ch - 10
                            in if (lhd * (cwd / lwd)) > chd
                                  then chd / lhd
                                  else cwd / lwd

scaleMap :: Double
         -> (DElem, (Int, Int), (Int, Int))
         -> (DElem, (Double, Double), (Double, Double))
scaleMap scale (e, (x,y), (xo,yo)) = (e, (f x, f y), (f xo, f yo))
  where f a = scale * (fromIntegral a)

nmap :: (a -> b) -> (a,a) -> (b,b)
nmap f (x,y) = (f x, f y)
