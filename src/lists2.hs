{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

-- import Control.Monad
import Data.Default
-- import Data.Text (Text)
import JavaScript.Canvas
import JavaScript.JQuery hiding (Event)
import GHCJS.Types
import GHCJS.Foreign
-- import GHCJS.Types
-- import GHCJS.Foreign
import Reactive.Banana
import Reactive.Banana.Frameworks

main = do
  ctx <- getContext =<< indexArray 0 . castRef =<< select "#theCanvas"
  btn <- select "#theButton"
  bEvent <- wireButton btn
  network <- compile (netDesc bEvent ctx)
  actuate network

netDesc :: Frameworks t => AddHandler () -> Context -> Moment t ()
netDesc addButtonEvent ctx = do
  eClicks <- fromAddHandler addButtonEvent
  let bCount = accumB 0 $ (\x -> x+1) <$ eClicks
  eCountChange <- changes bCount
  reactimate' $ fmap (\n -> drawList n ctx) <$> eCountChange

wireButton :: JQuery -> IO (AddHandler ())
wireButton button = do
  (addHandler, fire) <- newAddHandler
  let handler _ = fire ()
  click handler def button
  return addHandler

drawList :: Int -> Context -> IO ()
drawList i ctx = do
  save ctx
  clearRect 0 0 600 200 ctx 
  restore ctx
  r i ctx

r i ctx = 
  let ii = fromIntegral i
  in if i > 0
     then drawRect (105*ii) 100.0 ctx >> r (i-1) ctx
     else return ()

drawRect :: Double -> Double -> Context -> IO ()
drawRect x y ctx = do
  save ctx
  translate x y ctx
  strokeRect (-50) (-50) 100 100 ctx
  restore ctx
