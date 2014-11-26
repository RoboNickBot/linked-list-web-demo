{-# LANGUAGE CPP, OverloadedStrings #-}
module Demo.JS ( readInputState
               , writeInputState
               , mkCanvas ) where

import Control.Monad
import JavaScript.JQuery
import Data.Text (pack, unpack, Text)
import qualified Data.Map as M (empty, insert)

import Demo.Types

sRandomButton = select "#randomButton"
sCellsDiv = select "#boxbox"
sDrawButton = select "#buttonSpot"
sHeadInput = select "#head"
sCanvasBox = select "#drawingbox"
sCanvas = select "#theCanvas" -- dont forget to make it!
sCellNum i = select (pack (template (cellName i)))
  where template n = "<div class=\"outer\"><div class=\"inner\">" 
                     ++ (show i) 
                     ++ "</div><input id=\"" 
                     ++ n 
                     ++ "\" type=\"text\" name=\"a\" /></div>"
        
cellName :: Int -> String
cellName i = "#hey" ++ (show i)

-- returns (the old state, the new state)
readInputState :: InputState -> IO (InputState, InputState)
readInputState s = do h <- getHead
                      m <- getMemSt (startIndex s) (cellCount s)
                      return (s, s { headVal = h, memVals = m })

getHead :: IO String
getHead = fmap unpack (getVal =<< sHeadInput)

getMemSt :: Int -> Int -> IO MemSt
getMemSt start size = fmap mkMemSt (r start)
  where r i = if i < (start + size)
                 then do c <- readCell i
                         fmap (c:) (r (i+1))  --liftM (:) (readCell i) (r (i+1))
                 else return []

writeInputState :: InputState -> IO ()
writeInputState (InSt i s h m) = mkBoxes i s m >> setHead h

setHead :: String -> IO ()
setHead h = sHeadInput >>= setVal (pack h) >> return ()

readMemSt :: [Cell] -> MemSt
readMemSt = foldr (\(i,s) -> M.insert i (i,s)) M.empty

readCell :: Int -> IO Cell
readCell i = let name = pack (cellName i)
               in fmap ((,) i) . fmap unpack $ (getVal =<< select name)

writeCell :: Int -> String -> IO ()
writeCell i s = select (pack (cellName i)) >>= setVal (pack s) >> return ()

mkBoxes :: Int -> Int -> MemSt -> IO ()
mkBoxes start size m = clear >> r (start + size) size
  where r :: Int -> Int -> IO ()
        r n i = if i > 0
                   then do print $ "making box number " ++ (show i) 
                           box <- sCellNum (n - i)
                           parent <- sCellsDiv
                           appendJQuery box parent
                           writeCell i (stringAtIndex i m)
                           r n (i - 1)
                   else return ()
        clear :: IO ()
        clear = sCellsDiv >>= children >>= remove >> return ()

getCanvasDimensions :: IO (Int,Int)
getCanvasDimensions = do
  sh <- getHeight =<< select "#s"
  ah <- getHeight =<< select "#a"
  bh <- getHeight =<< select "#b"
  ch <- getHeight =<< select "#c"
  dw <- getWidth =<< select "#drawingbox"
  let h = floor $ sh - ah - bh - ch - 170
      w = floor $ dw - 13 -- not sure why i need this...
  return (w,h)
  
mkCanvas :: IO ()
mkCanvas = do
  (w,h) <- getCanvasDimensions
  p <- sCanvasBox
  c <- select $ pack $ "<canvas id=\"theCanvas\" width=\""
                       ++ show w
                       ++ "\" height=\""
                       ++ show h
                       ++ "\"></canvas>"
  appendJQuery c p
  return ()
