--
-- This is a demo of Elm
--
-- The goal here is to properly arrange some elements on the
-- page, keeping everything appropriately spaced and sized
--

import String as Str
import Window
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

-- Configurables
defaultListSize : Int
defaultListSize = 5

marginH : Int
marginH = 30

marginW : Int
marginW = 30

boxColor : Color
boxColor = rgba 150 90 240 0.8

pageColor : Color
pageColor = rgba 220 220 256 0.8 

-- ---------------------------------------
-- Intermission, featuring Markdown
--
--
mainHeader : Element
mainHeader = [markdown|

Let's make a list!
==================

Just type a space-separated list of things in the text-box below

(also, take a look at the [source code](lists.elm) for this page. It's nice!)

|]
--
--
-- End of Markdown stuff
-- ---------------------------------------

-- Functions
main : Signal Element
main = lift2 page content.signal Window.dimensions 

content : Input Field.Content
content = input Field.noContent

page : Field.Content -> (Int, Int) -> Element
page fieldContent (w,h) = color pageColor <|
  container w h midTop (scene fieldContent (w-marginW, h-marginH))

scene : Field.Content -> (Int, Int) -> Element
scene fieldContent (w,h) = flow down
    [ mainHeader
    , color white <| Field.field Field.defaultStyle content.handle identity "..."
        fieldContent
    , collage w (w // defaultListSize) 
        (buildTree (w-50) fieldContent.string)
    ]
  

marginScale : Float -> Element -> Element
marginScale f el =
  let (w,h) = sizeOf el
  in  collage w h [ scale f <| toForm el ]

buildTree : Int -> String -> [Form]
buildTree w ss =
  if Str.isEmpty ss
     then []
     else let ws = Str.words ss
              i = max defaultListSize (length ws)
              s = (w // i)
          in boxes (w-s) s i ws

boxes : Int -> Int -> Int -> [String] -> [Form]
boxes w s i css = case css of
  (c::cs) ->
    let offset = ((toFloat w) / 2 ) - (toFloat ((i-1) * s))
    in (moveX offset (box s c)) :: (boxes w s (i-1) cs)
  [] -> []

box : Int -> String -> Form 
box s cs = 
  let sf = 0.7 * (toFloat s)
  in toForm (collage s s
              [ (filled boxColor (rect sf sf))
              , toForm (plainText cs)
              ])
