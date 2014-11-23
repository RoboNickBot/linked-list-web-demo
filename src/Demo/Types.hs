module Demo.Types ( InputState
                  , DType (Box, Arrow, LoopBack)
                  , DElem
                  , Diagram
                  , MemSt
                  , Cell
                  ) where

import Data.Map (Map)

{- Here's a map of how these types are used:

   InputState ---> Diagram ---> Layout ----*----> Drawable
                                          /
                 (canvas dimensions) ----*
-}

-- (Cell Index, Cell contents)
type Cell = (Int, String)
-- Map (Cell Index) Cell
type MemSt = Map Int Cell

{- InputState: The current state of all the inputs on the
               the webpage right now
   (Head Index Box, Map of values in memory boxes) -}
type InputState = (String, MemSt)

data DType = Box | Arrow | LoopBack Int deriving ( Show, Eq )
type DElem = (DType, String, String)

{- Diagram: The actual list derived from the InputState, with
            each element holding the data that should ultimately
            be draw with it (values, indexes, etc.) -}
type Diagram = [DElem]

{- Layout: A list of objects that will be drawn on-screen with
           their positions and sizes, all in terms of a
           unit-size -}
--type Layout = undefined

{- Drawable: Simply the layout with actual values computed for
             the positions and sizes. This is handed to a
             function that will draw them with GHCJS-Canvas -}
--type Drawable = undefined


