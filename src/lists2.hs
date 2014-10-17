{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

-- import Control.Monad
import Data.Default
import qualified Data.Text as T(empty, pack, unpack, Text)
import JavaScript.Canvas
import JavaScript.JQuery hiding (Event)
import GHCJS.Types
import GHCJS.Foreign
-- import GHCJS.Types
-- import GHCJS.Foreign
import Reactive.Banana
import Reactive.Banana.Frameworks

main = do
  box <- select "#theTextBox"
  ctx <- getContext =<< indexArray 0 . castRef =<< select "#theCanvas"
  tEvent <- wireTextBox box
  network <- compile (netDesc tEvent ctx)
  actuate network

netDesc :: Frameworks t 
        => AddHandler (T.Text)
        -> Context 
        -> Moment t ()
netDesc addBoxEvent ctx = do
  eText <- fromAddHandler addBoxEvent
  let bText = stepper T.empty eText
  let bString = T.unpack <$> bText
  eStringChange <- changes bString
  reactimate' $ fmap (\ss -> drawList ss ctx) <$> eStringChange

wireTextBox :: JQuery -> IO (AddHandler T.Text)
wireTextBox box = do
  (addHandler, fire) <- newAddHandler
  text <- getVal box
  let handler _ = fire =<< getVal box
  keyup handler def box
  return addHandler

drawList :: String -> Context -> IO ()
drawList ss ctx = do
  let ws = words ss
  save ctx
  clearRect 0 0 700 400 ctx 
  restore ctx
  r (reverse ws) (length ws) (min 200 (700 / (fromIntegral $ length ws))) ctx

r :: [String] -> Int -> Double -> Context -> IO ()
r (w:ws) l d ctx = 
  let ii = fromIntegral l
  in if l > 0
     then drawRect w d ((d*(ii-1))+(d/2)) 100 ctx >> r ws (l-1) d ctx
     else return ()
r _ _ _ _ = return ()

drawRect :: String -> Double -> Double -> Double -> Context -> IO ()
drawRect w d x y ctx = do
  save ctx
  translate x y ctx
  font "20px Ariel" ctx
  fillText (T.pack w) 0 0 ctx
  strokeRect (-(d/2)+2) (-(d/2)+2) (d-4) (d-4) ctx
  restore ctx
