module Demo.Types ( InputState (..)
                  , emptyInput
                  , emptyCell
                  , stringAtIndex
                  , DType (Box, Arrow, LoopBack)
                  , DElem
                  , Diagram
                  , Layout
                  , LayoutD
                  , LElem
                  , MemSt
                  , mkMemSt
                  , Cell
                  ) where

import qualified Data.Map as M (Map, empty, lookup, insert)

{- Here's a map of how these types are used:

   InputState ---> Diagram ---> Layout ----*----> Drawable
                                          /
                 (canvas dimensions) ----*
-}

-- (Cell Index, Cell contents)
type Cell = (Int, String)
-- M.Map (Cell Index) Cell
type MemSt = M.Map Int Cell

mkMemSt :: [Cell] -> MemSt
mkMemSt = foldr (\(i,s) -> M.insert i (i,s)) M.empty

{- InputState: The current state of all the inputs on the
               the webpage right now -}
data InputState = InSt { startIndex :: Int
                       , cellCount :: Int
                       , headVal :: String
                       , memVals :: MemSt
                       } deriving ( Show, Eq )

emptyInput :: Int -> Int -> InputState
emptyInput start size = InSt start size "" M.empty

emptyCell :: Int -> Cell
emptyCell n = (n, "")

stringAtIndex :: Int -> MemSt -> String
stringAtIndex i m = case M.lookup i m of
                      Just (_,s) -> s
                      _ -> ""

data DType = Box | Arrow | LoopBack Int deriving ( Show, Eq )
type DElem = (DType, String, String)

{- Diagram: The actual list derived from the InputState, with
            each element holding the data that should ultimately
            be draw with it (values, indexes, etc.) -}
type Diagram = [DElem]


type LElem = (DElem, (Int, Int), (Int, Int))

{- Layout: A list of objects that will be drawn on-screen with
           their positions and sizes, all in terms of a
           unit-size -}
type Layout = [LElem]

type LayoutD = [(DElem, (Double, Double), (Double, Double))]
