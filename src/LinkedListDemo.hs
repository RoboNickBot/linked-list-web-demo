{-# LANGUAGE CPP, OverloadedStrings #-}

import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Default (def)
import JavaScript.JQuery hiding (Event)

import Demo.Types
import Demo.Links
import Demo.JS


main = initializePage
       --n <- buildNetwork
       --actuate n

initializePage = writeInputState (emptyInput 5 20) >> mkCanvas

buildNetwork = undefined
