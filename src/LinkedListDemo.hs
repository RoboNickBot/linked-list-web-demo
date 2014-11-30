{-# LANGUAGE CPP, OverloadedStrings #-}

import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Default (def)
import JavaScript.JQuery hiding (Event)

import Demo.Types
import Demo.Links
import Demo.JS


main = do initializePage
          ss <- mkSources
          wireButton (fst ss) sDrawButton readInputState
          wireButton (snd ss) sRandomButton mkRandomInput
          n <- compile (mkNetwork ss)
          actuate n

initializePage = writeInputState (emptyInput 5 20) >> mkCanvas

wireButton (addHandler, fire) button f = do 
  let handler _ = f >>= fire
  b <- button
  click handler def b

mkSources = (,) <$> newAddHandler <*> newAddHandler

addHandler = fst
fire = snd

mkNetwork (drawSource, randomSource) = do 
  eDraws <- fromAddHandler (addHandler drawSource)
  eRandoms <- fromAddHandler (addHandler randomSource)
  let eInputs = eDraws `union` eRandoms
      --bInputState :: Behavior t InputState
      bInputState = stepper (emptyInput 5 20) eInputs
      
  --reactimate' $ fmap (\is -> process is) eISChanged
  --reactimate' <$> (fmap (fmap process) eISChanged)
  reactimate (fmap (\a -> mkCanvas >> process a) eDraws)
  --reactimate (fmap (\_ -> fmap process bInputState) eDraws)

process :: InputState -> IO ()
process = displayOutput . fmap mkLayout . parseInput
