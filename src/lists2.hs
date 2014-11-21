{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

import Demo.Links (compute, randomList)
import Data.Default
import qualified Data.Text as T(empty, pack, unpack, Text)
import JavaScript.Canvas
import JavaScript.JQuery hiding (Event)
import GHCJS.Types
import GHCJS.Foreign
import Reactive.Banana
import Reactive.Banana.Frameworks

main = do
  dim <- getCanvasDimensions
  cs <- select "#drawingbox"
  makeCanvas dim cs
  buttonspot <- select "#buttonSpot"
  boxbox <- select "#boxbox"
  randomButton <- select "#randomButton"
  ctx <- getContext =<< indexArray 0 . castRef =<< select "#theCanvas"
  button <- makeButton buttonspot
  -- if you change the 10 here, change the 10 in makeBoxes' def too!
  bs <- makeBoxes 10 boxbox []
  tEvent <- wireTextBoxes bs button
  rEvent <- wireRandom randomButton
  network <- compile (netDesc tEvent rEvent ctx bs)
  actuate network

makeCanvas :: (Int, Int) -> JQuery -> IO JQuery
makeCanvas (w,h) p = do
  c <- select $ T.pack $ "<canvas id=\"theCanvas\" width=\""
                         ++ show w
                         ++ "\" height=\""
                         ++ show h
                         ++ "\"></canvas>"
  appendJQuery c p
  select "#theCanvas"

killCanvas :: IO ()
killCanvas = do
  c <- select "#theCanvas"
  remove c
  return ()

resizeCanvas :: IO ()
resizeCanvas = do
  killCanvas
  dim <- getCanvasDimensions
  cs <- select "#drawingbox"
  makeCanvas dim cs
  return ()

makeButton :: JQuery -> IO JQuery
makeButton parent = do
  button <- select "#drawButton"
  return button

netDesc :: Frameworks t 
        => AddHandler ([String])
        -> AddHandler ([String])
        -> Context 
        -> [JQuery]
        -> Moment t ()
netDesc addDButtonEvent addRButtonEvent ctx bs = do
  eStrings <- fromAddHandler addDButtonEvent
  eRandom <- fromAddHandler addRButtonEvent
  let bStrings = stepper [] eStrings
  let bR = stepper [] eRandom
  stringChange <- changes bStrings
  randoLists <- changes bR
  reactimate' $ fmap (\ss -> drawList (compute ss) ctx) <$> stringChange
  reactimate' $ fmap (\ss -> fillList ss bs) <$> randoLists

wireTextBox :: JQuery -> JQuery -> IO (AddHandler T.Text)
wireTextBox box button = do
  (addHandler, fire) <- newAddHandler
  text <- getVal box
  let handler _ = fire =<< getVal box
  click handler def button
  return addHandler

wireTextBoxes :: [JQuery] -> JQuery -> IO (AddHandler [String])
wireTextBoxes bs button = do
  (addHandler, fire) <- newAddHandler
  let handler _ = fire =<< getVals bs
  click handler def button
  return addHandler

wireRandom :: JQuery -> IO (AddHandler [String])
wireRandom rb = do
  (addHandler, fire) <- newAddHandler
  let handler _ = fire =<< randomList
  click handler def rb
  return addHandler

fillList :: [String] -> [JQuery] -> IO ()
fillList (s:ss) (b:bb) = do
  setVal (T.pack s) b
  fillList ss bb
fillList _ _ = return ()

getVals :: [JQuery] -> IO [String]
getVals (b:bs) = do
  t <- getVal b
  let s = T.unpack t
  ss <- getVals bs
  -- print $ concat (s:ss)
  return (s:ss)
  --s <- getVal b
  --return $ (:) s $ =<< getVals bs
getVals _ = return []

drawList :: [String] -> Context -> IO ()
drawList ws ctx = do
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

makeBoxes :: Int -> JQuery -> [JQuery] -> IO [JQuery]
makeBoxes n p bs = 
  if n > 0
  then do
    b <- select $ T.pack $ "<div class=\"outer\"><div class=\"inner\">"++ show (10-n) ++"</div><input id=\"hey"++ (show n) ++"\" type=\"text\" name=\"a\" /></div>"
    appendJQuery b p
    c <- select $ T.pack $ "#hey"++(show n)
    makeBoxes (n-1) p (c:bs)
  else return (reverse bs)

getCanvasDimensions :: IO (Int,Int)
getCanvasDimensions = do
  sh <- getHeight =<< select "#s"
  ah <- getHeight =<< select "#a"
  bh <- getHeight =<< select "#b"
  ch <- getHeight =<< select "#c"
  dw <- getWidth =<< select "#drawingbox"
  let h = floor $ sh - ah - bh - ch - 200
      w = floor $ dw - 13 -- not sure why i need this...
  return (w,h)
