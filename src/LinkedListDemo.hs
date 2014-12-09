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
          
          -- init and connect reactive event-sources
          (draw, rando, gener, clicks, keys) <- mkSources 
          wireButton draw sDrawButton (cullErrors
                                       >> readInputState)
          wireButton rando sRandomButton (cullErrors 
                                          >> mkCanvas
                                          >> mkRandomInput
                                          >> readInputState)
          wireButton gener sCellGen (cullErrors 
                                     >> generateCells
                                     >> readInputState)
          wireClicks clicks -- does nothing for now
          wireKeys keys readInputState
          
          -- build and "actuate" the reactive event network
          n <- compile (mkNetwork (draw, rando, gener, clicks, keys))
          actuate n

generateCells :: IO ()
generateCells = 
  do genInfo <- getGenInfo
     case checkGenInfo =<< genInfo of
       Right (start,size) -> initializePage (start,size)
       Left err -> printHighError err

{- Tests number of cells and starting index to make sure they're valid.
   Readability as Ints was already checked for in JS.hs code -}
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

-- Events responding to keypresses in the memory editing area
wireKeys (addHandler, fire) f = do let handler _ = f >>= fire
                                   box <- sLowerControls
                                   keyup handler def box

-- convenience function for wiring all the buttons
wireButton (addHandler, fire) button f = do 
  let handler _ = f >>= fire
  b <- button
  click handler def b

-- sources are in the IO monad, so we have to do this?
mkSources = do a <- newAddHandler
               b <- newAddHandler
               c <- newAddHandler
               d <- newAddHandler
               e <- newAddHandler
               return (a,b,c,d,e)

-- convenience
addHandler = fst
fire = snd

{- And now the fun stuff, describes the reactive "signal graph" of
    events and behaviors, mainly used here to control when cells
    are highlighted to show that they have been edited -}
mkNetwork ( drawSource
          , randomSource
          , genSource
          , clickSource
          , keySource ) = do 

  eDraws <- fromAddHandler (addHandler drawSource)
  eRandoms <- fromAddHandler (addHandler randomSource)
  eGens <- fromAddHandler (addHandler genSource)
  eKeys <- fromAddHandler (addHandler keySource)
  
  let -- some useful collections of event-sources
      eResets = eRandoms `union` eGens
      eInputs = eRandoms `union` eGens `union` eKeys
      
      -- convenience for processing LastInputStates
      bNothing :: Behavior t (Maybe InputState)
      bNothing = pure Nothing
      -- clicking 'draw' should only count if it actually draws
      bTest :: Behavior t (InputState -> Maybe InputState)
      bTest = pure (\inState -> case parseInput inState of
                                  Left _ -> Nothing
                                  Right _ -> Just inState)

      bInputState = stepper (emptyInput 5 20) eInputs 
      bLastInputState = 
        stepper Nothing
                ((bTest <@> eDraws) `union` (bNothing <@ eResets))
      bDirty = mismatches <$> bInputState <*> bLastInputState
  
  cIn <- changes bInputState
  cLIn <- changes bLastInputState
  cDirty <- changes bDirty

  -- Draw the list!
  reactimate (fmap (\a -> mkCanvas >> process a) eDraws)
  -- Mark the "dirty" edited cells (or unmark them if clean)
  reactimate' (fmap (\d -> mark d >> return ()) <$> cDirty)

  -- (These are for debugging purposes and print only to the console)
  reactimate' $ fmap (\d -> print ("InputState: " ++ show d)) <$> cIn
  reactimate' $ fmap (\d -> print ("LastState: " ++ show d)) <$> cLIn


process :: InputState -> IO ()
process = displayOutput . fmap mkLayout . parseInput
