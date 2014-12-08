{-# LANGUAGE CPP, OverloadedStrings #-}

import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Default (def)
import JavaScript.JQuery hiding (Event)

import Demo.Random
import Demo.Types
import Demo.Links
import Demo.JS

defaultHeadIndex = 23
defaultNumCells = 14

main = do initializePage (defaultHeadIndex, defaultNumCells)
          (draw, rando, gener) <- mkSources
          wireButton draw sDrawButton readInputState
          wireButton rando sRandomButton mkRandomInput
          wireButton gener sCellGen generateCells
          n <- compile (mkNetwork (draw, rando, gener))
          actuate n

generateCells :: IO ()
generateCells = do (start,size) <- getGenInfo
                   initializePage (read start, read size)

initializePage (start,size) = 
  placeValues start size
  >> writeInputState (emptyInput start size) 
  >> mkCanvas



wireButton (addHandler, fire) button f = do 
  let handler _ = f >>= fire
  b <- button
  click handler def b

mkSources = do a <- newAddHandler
               b <- newAddHandler
               c <- newAddHandler
               return (a,b,c)

addHandler = fst
fire = snd

mkNetwork (drawSource, randomSource, genSource) = do 
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
