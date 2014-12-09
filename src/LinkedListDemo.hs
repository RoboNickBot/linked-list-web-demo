{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

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
          (draw, rando, gener, clicks, keys) <- mkSources
          wireButton draw sDrawButton (cullErrors >> readInputState)
          wireButton rando sRandomButton (cullErrors 
                                          >> mkCanvas
                                          >>mkRandomInput
                                          >> readInputState)
          wireButton 
            gener 
            sCellGen 
            (cullErrors >> generateCells >> readInputState)
          wireClicks clicks
          wireKeys keys readInputState
          n <- compile (mkNetwork (draw, rando, gener, clicks, keys))
          actuate n

generateCells :: IO ()
generateCells = 
  do genInfo <- getGenInfo
     case checkGenInfo =<< genInfo of
       Right (start,size) -> initializePage (start,size)
       Left err -> printHighError err

checkGenInfo :: (Int,Int) -> Either String (Int,Int)
checkGenInfo (i,s) 
  | i < 0 = Left "Starting Index cannot be negative"
  | s < 2 || s > 100 = Left "Number of Cells must be between 2 and 100"
  | otherwise = Right (i,s)

initializePage (start,size) = 
  placeValues start size
  >> writeInputState (emptyInput start size) 
  >> mkCanvas

wireClicks _ = return () -- maybe implement this later?
wireKeys (addHandler, fire) f = do let handler _ = f >>= fire
                                   box <- sLowerControls
                                   keyup handler def box

wireButton (addHandler, fire) button f = do 
  let handler _ = f >>= fire
  b <- button
  click handler def b

mkSources = do a <- newAddHandler
               b <- newAddHandler
               c <- newAddHandler
               d <- newAddHandler
               e <- newAddHandler
               return (a,b,c,d,e)

addHandler = fst
fire = snd

mkNetwork (drawSource, randomSource, genSource, clickSource, keySource) = do 
  eDraws <- fromAddHandler (addHandler drawSource)
  eRandoms <- fromAddHandler (addHandler randomSource)
  eGens <- fromAddHandler (addHandler genSource)
  eKeys <- fromAddHandler (addHandler keySource)
  let eResets = eRandoms `union` eGens
      eInputs = eRandoms `union` eGens `union` eKeys
      --bInputState :: Behavior t InputState 
      bInputState = stepper (emptyInput 5 20) eInputs
      eDrawnInputState = bInputState <@ eDraws
      bLastInputState = stepper (emptyInput 5 20)
                                (eDrawnInputState `union` eResets)
      bDirty = mismatches <$> bInputState <*> bLastInputState
  cIn <- changes bInputState
  cLIn <- changes bLastInputState
  cDirty <- changes bDirty


  --reactimate' $ fmap (\is -> process is) eISChanged
  --reactimate' <$> (fmap (fmap process) eISChanged)
  reactimate (fmap (\a -> mkCanvas >> process a) eDraws)
  reactimate' $ fmap (\d -> mark d >> print (show d) >> return ()) <$> cDirty
  reactimate' $ fmap (\d -> print ("InputState: " ++ show d)) <$> cIn
  reactimate' $ fmap (\d -> print ("LastState: " ++ show d)) <$> cLIn
  --reactimate (fmap (\_ -> fmap process bInputState) eDraws)

process :: InputState -> IO ()
process = displayOutput . fmap mkLayout . parseInput
