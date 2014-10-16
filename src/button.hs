{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import JavaScript.JQuery hiding (Event)
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
  body <- select "body"
  buttonEvent <- makeButton "Click Me!" body
  counterDiv <- select "<div />"
  appendJQuery counterDiv body
  network <- compile (netDesc buttonEvent counterDiv)
  actuate network

netDesc :: Frameworks t
        => AddHandler Int
        -> JQuery
        -> Moment t ()
netDesc addButtonEvent field = do
  eClicks <- fromAddHandler addButtonEvent
  let bCount = accumB 0 $ (\x -> x+1) <$ eClicks
  eCountChange <- changes bCount
  reactimate' $ fmap (\n -> void $ setText (T.pack . show $ n) field)
             <$> eCountChange

makeButton :: Text -> JQuery -> IO (AddHandler Int)
makeButton label parent = do
  (addHandler, fire) <- newAddHandler
  button <- select "<button />"
  setText label button
  appendJQuery button parent
  let handler _ = fire 0
  on handler "click" def button
  return addHandler
