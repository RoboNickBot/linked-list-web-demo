{-# LANGUAGE CPP, OverloadedStrings #-}
module Demo.JS ( getInputState
               , mkBoxes
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
sCellNum i = select (pack (template name))
  where name = "#hey" ++ (show i)
        template n = "<div class=\"outer\"><div class=\"inner\">" 
                     ++ (show i) 
                     ++ "</div><input id=\"" 
                     ++ n 
                     ++ "\" type=\"text\" name=\"a\" /></div>"

getInputState :: Int -> IO InputState
getInputState n = do h <- getHead
                     m <- getMemSt n
                     return (h,m)

getHead :: IO String
getHead = fmap unpack (getVal =<< sHeadInput)

getMemSt :: Int -> IO MemSt
getMemSt n = fmap mkMemSt (r 0)
  where r i = if i < n
                 then do c <- mkCell i
                         fmap (c:) (r (i+1))  --liftM (:) (mkCell i) (r (i+1))
                 else return []

mkMemSt :: [Cell] -> MemSt
mkMemSt = foldr (\(i,s) -> M.insert i (i,s)) M.empty

mkCell :: Int -> IO Cell
mkCell i = let name = pack ("#hey" ++ (show i))
           in fmap ((,) i) . fmap unpack $ (getVal =<< select name)

mkBoxes :: Int -> IO ()
mkBoxes size = print "making boxes!" >> clear >> r size size
  where r :: Int -> Int -> IO ()
        r n i = if i > 0
                   then do print $ "making box number " ++ (show i) 
                           box <- sCellNum (n - i)
                           parent <- sCellsDiv
                           appendJQuery box parent
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
